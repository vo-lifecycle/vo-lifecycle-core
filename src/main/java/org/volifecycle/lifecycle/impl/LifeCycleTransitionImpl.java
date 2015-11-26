package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.volifecycle.utils.CommonUtils.implode;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.dozer.DozerBeanMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.common.AbstractLifeCycle;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.LogEventManagerImpl;
import org.volifecycle.event.vo.Event;
import org.volifecycle.event.vo.LifeCycleTransitionEvent;
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleActionStorage;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleCompositeAction;
import org.volifecycle.lifecycle.LifeCycleTransition;

/**
 * Transition implementation
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public class LifeCycleTransitionImpl<T> extends AbstractLifeCycle<T> implements LifeCycleTransition<T> {
    private static final Logger LOGGER = LoggerFactory.getLogger(LifeCycleTransitionImpl.class);

    /**
     * List of composite actions.
     */
    protected List<LifeCycleAction<T>> actions;

    /**
     * Action storage.
     */
    protected LifeCycleActionStorage<T> actionStorage;

    /**
     * The id.
     */
    protected String id;

    /**
     * auto | manual
     */
    protected String type;

    /**
     * Description
     */
    protected String description;

    /**
     * List of targeted states.
     */
    protected List<String> targetStates;

    /**
     * Setting with true if you want stop when a predicate failed.
     */
    protected Boolean stopIfFailed;

    /**
     * Dozer bean mapping.
     */
    protected DozerBeanMapper dozerBeanMapper;

    /**
     * Constructor.
     */
    public LifeCycleTransitionImpl() {
        dozerBeanMapper = new DozerBeanMapper();
    }

    /**
     * @return the actions
     */
    public List<LifeCycleAction<T>> getActions() {
        return actions;
    }

    /**
     * @param actions
     *            the actions to set
     */
    public void setActions(List<LifeCycleAction<T>> actions) {
        this.actions = actions;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return description;
    }

    /**
     * @return the stopIfFailed
     */
    public Boolean getStopIfFailed() {
        return stopIfFailed;
    }

    /**
     * @param stopIfFailed
     *            the stopIfFailed to set
     */
    public void setStopIfFailed(Boolean stopIfFailed) {
        this.stopIfFailed = stopIfFailed;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the actionStorage
     */
    public LifeCycleActionStorage<T> getActionStorage() {
        return actionStorage;
    }

    /**
     * @param actionStorage
     *            the actionStorage to set
     */
    public void setActionStorage(LifeCycleActionStorage<T> actionStorage) {
        this.actionStorage = actionStorage;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getTargetStates() {
        return targetStates;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setTargetStates(List<String> targetStates) {
        this.targetStates = targetStates;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager) {
        return changeState(valueObject, adapter, evtManager, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedActions) {
        return changeState(valueObject, adapter, evtManager, forcedActions, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedActions, Map<String, Object> storage) {
        String rtn = Boolean.TRUE.toString();
        Map<String, String> additionnalInformations = null;

        Map<String, Object> actionStorageResult = null;
        if (null != actionStorage) {
            actionStorageResult = actionStorage.getActionStorageResult(valueObject);
        } else if (null == actionStorageResult) {
            actionStorageResult = new HashMap<String, Object>();
        }

        if (null == evtManager) {
            evtManager = new LogEventManagerImpl();
        }

        if (null == storage) {
            storage = new HashMap<String, Object>();
        }
        storage.putAll(actionStorageResult);

        List<Event> listEvent = new ArrayList<Event>();
        if (isNotEmpty(actions)) {
            for (LifeCycleAction<T> action : actions) {
                boolean filter = false;

                // Searching if is an action to ignore
                if (isNotEmpty(forcedActions)) {
                    for (String idForcedAction : forcedActions) {
                        if (null != action.getId() && idForcedAction.equalsIgnoreCase(action.getId())) {
                            LOGGER.info("Skipping result : " + action.getId());
                            filter = true;
                            break;
                        }
                    }
                }

                LOGGER.info("Testing action : " + action.getId());

                additionnalInformations = action.getAdditionnalInformations();
                List<String> failedSimpleActions = new ArrayList<String>();
                List<String> forcedActionsInReality = new ArrayList<String>();
                String result = null;
                LifeCycleCompositeAction<T> compositeAction = null;
                if (action instanceof LifeCycleCompositeAction<?>) {
                    compositeAction = (LifeCycleCompositeAction<T>) action;
                    result = compositeAction.getResult(valueObject, failedSimpleActions, storage, forcedActions, forcedActionsInReality);
                } else {
                    result = action.getResult(valueObject, storage);
                }

                LOGGER.info("Action result : id = " + action.getId() + ", result = " + result);

                if (null == result || Boolean.FALSE.toString().equalsIgnoreCase(result)) {
                    LOGGER.debug("Case 1.X");

                    if (!filter) {
                        LOGGER.debug("Case 1.1");

                        rtn = Boolean.FALSE.toString();
                        String message = buildMessageAction("Failed", null, valueObject, adapter, action, failedSimpleActions);
                        buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FAILED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));

                        if (null != stopIfFailed && stopIfFailed) {
                            LOGGER.info("stopIfFailed is enabled : skipping next actions...");
                            break;
                        }
                    } else if (null != compositeAction) {
                        LOGGER.debug("Case 1.2");

                        rtn = compositeAction.getTargetState();
                        String message = buildMessageAction("Forced", null, valueObject, adapter, action, failedSimpleActions);
                        buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));
                        break;
                    } else if (isNotEmpty(targetStates) && targetStates.size() == 1) {
                        LOGGER.debug("Case 1.3");

                        rtn = targetStates.get(0);
                        String message = buildMessageAction("Forced", "only one target state", valueObject, adapter, action, failedSimpleActions);
                        buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));
                    } else {
                        LOGGER.debug("Case 1.4");

                        rtn = Boolean.FALSE.toString();
                        String message = buildMessageAction("Failed", "no target state", valueObject, adapter, action, failedSimpleActions);
                        buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FAILED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));

                        if (null != stopIfFailed && stopIfFailed) {
                            LOGGER.info("stopIfFailed is enabled : skipping next actions...");
                            break;
                        }
                    }
                } else if (null != compositeAction) {
                    LOGGER.debug("Case 2.X");

                    rtn = compositeAction.getTargetState();
                    if (isNotEmpty(forcedActionsInReality)) {
                        String message = buildMessageAction("Forced", null, valueObject, adapter, action, forcedActionsInReality);
                        buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), forcedActionsInReality));
                    }

                    break;
                } else if (!Boolean.FALSE.toString().equalsIgnoreCase(rtn)) {
                    LOGGER.debug("Case 3.X");

                    rtn = result;
                }
            }
        }

        if (Boolean.TRUE.toString().equalsIgnoreCase(rtn) && isNotEmpty(targetStates) && targetStates.size() == 1) {
            rtn = targetStates.get(0);
            LOGGER.info("[1] Transition result : id = " + this.getId() + ", result = " + rtn);
        } else if (isEmpty(targetStates) || null == rtn || Boolean.FALSE.toString().equals(rtn) || !targetStates.contains(rtn)) {
            if (!Boolean.FALSE.toString().equals(rtn) && !targetStates.contains(rtn)) {
                LOGGER.error("The state " + rtn + " is not expected by transition : id = " + this.getId() + ", targetStates = " + implode(",", targetStates));
            }

            rtn = Boolean.FALSE.toString();
            String message = "Failed transition : id=" + this.getId() + ", targetStates = " + implode(",", targetStates);
            Event evt = buildCustomEvent(valueObject, adapter, LifeCycleConstants.EVENT_TYPE_FAILED_TRANSITION, message, additionnalInformations, null, null);
            LifeCycleTransitionEvent trEvt = dozerBeanMapper.map(evt, LifeCycleTransitionEvent.class);
            trEvt.setSubActionsEvents(listEvent);

            evtManager.logEvent(trEvt);

            LOGGER.info("[2] Transition result : id = " + this.getId() + ", result = " + rtn);
        }

        return rtn;
    }

    /**
     * Build log message for a failed or forced action.
     * 
     * @param actionStatus
     * @param valueObject
     * @param adapter
     * @param action
     * @param failedSimpleActions
     * @return String
     */
    private String buildMessageAction(String actionStatus, String details, T valueObject, LifeCycleAdapter<T> adapter, LifeCycleAction<T> action, List<String> failedSimpleActions) {
        if (isNotEmpty(details)) {
            details = String.format(" (%s) ", details);
        } else {
            details = "";
        }

        return actionStatus + " action : " + action.getId() + details + ", sub actions : " + implode(",", failedSimpleActions) + ", object id = " + adapter.getId(valueObject) + ", object type = " + adapter.getType(valueObject);
    }

    /**
     * Build final list of failed actions.
     * 
     * @param actionId
     * @param failedSimpleActionIds
     * @return List<String>
     */
    private List<String> buildListFailedIds(String actionId, List<String> failedSimpleActionIds) {
        List<String> rtn = new ArrayList<String>();

        if (isNotEmpty(failedSimpleActionIds)) {
            rtn.addAll(failedSimpleActionIds);
        }

        if (!rtn.contains(actionId)) {
            rtn.add(actionId);
        }

        return rtn;
    }
}
