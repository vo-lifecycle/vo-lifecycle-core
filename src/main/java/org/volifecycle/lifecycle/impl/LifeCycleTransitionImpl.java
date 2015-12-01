package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.join;
import static org.volifecycle.common.LifeCycleConstants.EVENT_TYPE_FAILED_ACTION;
import static org.volifecycle.common.LifeCycleConstants.EVENT_TYPE_FAILED_TRANSITION;
import static org.volifecycle.common.LifeCycleConstants.EVENT_TYPE_FORCED_ACTION;
import static org.volifecycle.common.LifeCycleConstants.EVENT_TYPE_SUCCESS_TRANSITION;
import static org.volifecycle.common.LifeCycleConstants.KEY_ADDI_SOURCE_ACTION_ID;
import static org.volifecycle.event.EventBuilder.build;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.LogEventManagerImpl;
import org.volifecycle.event.vo.Event;
import org.volifecycle.event.vo.LifeCycleTransitionEvent;
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleActionStorage;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleCompositeAction;
import org.volifecycle.lifecycle.LifeCycleTransition;
import org.volifecycle.utils.DozerBeanMapperFactory;

/**
 * Transition implementation.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public class LifeCycleTransitionImpl<T> implements LifeCycleTransition<T> {
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
     * Additionnal informations
     */
    protected Map<String, String> additionnalInformations;

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
                boolean filter = isAnActionToIgnore(forcedActions, action);

                LOGGER.info("[changeState] Testing action : " + action.getId());

                additionnalInformations = action.getAdditionnalInformations();

                if (null == additionnalInformations) {
                    additionnalInformations = new HashMap<String, String>();
                }

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

                LOGGER.info("[changeState] Action result : id = " + action.getId() + ", result = " + result);
                additionnalInformations.put(KEY_ADDI_SOURCE_ACTION_ID, action.getId());

                if (null == result || Boolean.FALSE.toString().equalsIgnoreCase(result)) {
                    LOGGER.debug("[changeState] Case 1.X");

                    if (!filter) {
                        LOGGER.debug("[changeState] Case 1.1");

                        rtn = Boolean.FALSE.toString();
                        String message = buildMessageAction("Failed", null, valueObject, adapter, action, failedSimpleActions);
                        build(valueObject, adapter, EVENT_TYPE_FAILED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));

                        if (null != stopIfFailed && stopIfFailed) {
                            LOGGER.info("[changeState] stopIfFailed is enabled : skipping next actions...");
                            break;
                        }
                    } else if (null != compositeAction) {
                        LOGGER.debug("[changeState] Case 1.2");

                        rtn = compositeAction.getTargetState();
                        String message = buildMessageAction("Forced", null, valueObject, adapter, action, failedSimpleActions);
                        build(valueObject, adapter, EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));
                        break;
                    } else if (isNotEmpty(targetStates) && targetStates.size() == 1) {
                        LOGGER.debug("[changeState] Case 1.3");

                        rtn = targetStates.get(0);
                        String message = buildMessageAction("Forced", "only one target state", valueObject, adapter, action, failedSimpleActions);
                        build(valueObject, adapter, EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));
                    } else {
                        LOGGER.debug("[changeState] Case 1.4");

                        rtn = Boolean.FALSE.toString();
                        String message = buildMessageAction("Failed", "no target state", valueObject, adapter, action, failedSimpleActions);
                        build(valueObject, adapter, EVENT_TYPE_FAILED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), failedSimpleActions));

                        if (null != stopIfFailed && stopIfFailed) {
                            LOGGER.info("[changeState] stopIfFailed is enabled : skipping next actions...");
                            break;
                        }
                    }
                } else if (null != compositeAction) {
                    LOGGER.debug("[changeState] Case 2.X");

                    rtn = compositeAction.getTargetState();
                    if (isNotEmpty(forcedActionsInReality)) {
                        String message = buildMessageAction("Forced", null, valueObject, adapter, action, forcedActionsInReality);
                        build(valueObject, adapter, EVENT_TYPE_FORCED_ACTION, message, additionnalInformations, listEvent, buildListFailedIds(action.getId(), forcedActionsInReality));
                    }

                    break;
                } else if (!Boolean.FALSE.toString().equalsIgnoreCase(rtn)) {
                    LOGGER.debug("[changeState] Case 3.X");

                    rtn = result;
                }
            }
        }

        if (Boolean.TRUE.toString().equalsIgnoreCase(rtn) && isNotEmpty(targetStates) && targetStates.size() == 1) {
            rtn = targetStates.get(0);
            LOGGER.info("[changeState][1] Transition result : id = " + this.getId() + ", result = " + rtn);
            logTransitionEvent(EVENT_TYPE_SUCCESS_TRANSITION, valueObject, adapter, evtManager, rtn, additionnalInformations, listEvent);
        } else if (isEmpty(targetStates) || null == rtn || Boolean.FALSE.toString().equals(rtn) || !targetStates.contains(rtn)) {
            if (!Boolean.FALSE.toString().equals(rtn) && !targetStates.contains(rtn)) {
                LOGGER.error("[changeState] The state " + rtn + " is not expected by transition : id = " + this.getId() + ", targetStates = " + join(",", targetStates));
            }

            rtn = Boolean.FALSE.toString();
            logTransitionEvent(EVENT_TYPE_FAILED_TRANSITION, valueObject, adapter, evtManager, rtn, additionnalInformations, listEvent);
            LOGGER.info("[changeState][2] Transition result : id = " + this.getId() + ", result = " + rtn);
        } else {
            logTransitionEvent(EVENT_TYPE_SUCCESS_TRANSITION, valueObject, adapter, evtManager, rtn, additionnalInformations, listEvent);
            LOGGER.info("[changeState][3] Transition result : id = " + this.getId() + ", result = " + rtn);
        }

        return rtn;
    }

    /**
     * Logging transition event.
     * 
     * @param transitionState
     * @param valueObject
     * @param adapter
     * @param evtManager
     * @param rtn
     * @param additionnalInformations
     * @param listEvent
     */
    private void logTransitionEvent(String typeTransiton, T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, String rtn, Map<String, String> additionnalInformations, List<Event> listEvent) {
        String message = typeTransiton + " : id=" + this.getId() + ", targetStates=" + join(",", targetStates) + ", result=" + rtn;
        Event evt = build(valueObject, adapter, typeTransiton, message, additionnalInformations, null, null);
        LifeCycleTransitionEvent trEvt = DozerBeanMapperFactory.getInstance().map(evt, LifeCycleTransitionEvent.class);
        trEvt.setActionsEvents(listEvent);
        evtManager.logEvent(trEvt);
    }

    /**
     * Searching if it's an action to ignore.
     * 
     * @param forcedActions
     * @param action
     * @param filter
     * @return boolean
     */
    private boolean isAnActionToIgnore(List<String> forcedActions, LifeCycleAction<T> action) {
        if (isNotEmpty(forcedActions)) {
            for (String idForcedAction : forcedActions) {
                if (null != action.getId() && idForcedAction.equalsIgnoreCase(action.getId())) {
                    LOGGER.info("[isAnActionToIgnore] Skipping result : " + action.getId());
                    return true;
                }
            }
        }

        return false;
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

        return actionStatus + " action : " + action.getId() + details + ", sub actions : " + join(",", failedSimpleActions) + ", object id = " + adapter.getId(valueObject) + ", object type = " + adapter.getType(valueObject);
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

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getAdditionnalInformations() {
        return additionnalInformations;
    }

    /**
     * @param additionnalInformations
     *            the additionnalInformations to set
     */
    public void setAdditionnalInformations(Map<String, String> additionnalInformations) {
        this.additionnalInformations = additionnalInformations;
    }
}
