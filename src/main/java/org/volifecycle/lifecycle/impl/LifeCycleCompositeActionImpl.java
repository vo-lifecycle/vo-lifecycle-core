package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleCompositeAction;

/**
 * Implementation of composite action.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject
 */
public class LifeCycleCompositeActionImpl<T> implements LifeCycleCompositeAction<T> {
    private static final Logger LOGGER = LoggerFactory.getLogger(LifeCycleCompositeActionImpl.class);

    /**
     * Id which is used for forced the result of this action
     */
    protected String id;

    /**
     * Description
     */
    protected String description;

    /**
     * target state
     */
    protected String targetState;

    /**
     * List actions wich are executed by "and" predicate operator.
     */
    protected List<LifeCycleAction<T>> actions;

    /**
     * Setting with true if you want stop when a predicate failed.
     */
    protected Boolean stopIfFailed;

    /**
     * Additionnal informations to save.
     */
    protected Map<String, String> additionnalInformations;

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
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTargetState() {
        return targetState;
    }

    /**
     * @param targetState
     *            the targetState to set
     */
    public void setTargetState(String targetState) {
        this.targetState = targetState;
    }

    /**
     * {@inheritDoc}
     */
    @Override
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

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, List<String> failedPredicate) {
        return getResult(valueObject, failedPredicate, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, Map<String, Object> actionStorage) {
        return getResult(valueObject, null, actionStorage);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, List<String> failedSubActions, Map<String, Object> actionStorage) {
        return getResult(valueObject, null, actionStorage, null, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, List<String> failedSubActions, Map<String, Object> actionStorage, List<String> forcedActions, List<String> forcedActionsInReality) {
        String rtn = getTargetState();
        String result;

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

                result = action.getResult(valueObject, actionStorage);

                LOGGER.info("Action result : id = " + action.getId() + ", result = " + result);

                if (Boolean.FALSE.toString().equalsIgnoreCase(result)) {
                    if (filter) {
                        forcedActionsInReality.add(action.getId());
                        continue;
                    }

                    rtn = Boolean.FALSE.toString();

                    if (null != failedSubActions && !failedSubActions.contains(action.getId())) {
                        failedSubActions.add(action.getId());
                    }

                    if (null != stopIfFailed && stopIfFailed) {
                        LOGGER.info("stopIfFailed is enabled : skipping next actions...");
                        break;
                    }
                }
            }
        }

        return rtn;
    }
}
