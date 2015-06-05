package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;
import java.util.Map;

import org.volifecycle.common.AbstractLifeCycle;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleActionStorage;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChecker;
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
    /**
     * List of composite checkers.
     */
    protected List<LifeCycleChecker<T>> checkers;

    /**
     * Action storage.
     */
    protected LifeCycleActionStorage<T> actionStorage;

    /**
     * auto | manual
     */
    protected String type;

    /**
     * Description
     */
    protected String description;

    /**
     * {@inheritDoc}
     */
    @Override
    public List<LifeCycleChecker<T>> getCheckers() {
        return checkers;
    }

    /**
     * @param checkers
     *            the checkers to set
     */
    public void setCheckers(List<LifeCycleChecker<T>> checkers) {
        this.checkers = checkers;
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
    public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager) {
        return changeState(valueObject, adapter, evtManager, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedCheckers) {
        String rtn = LifeCycleConstants.TRUE;

        Map<String, Object> actionStorageResult = null;
        if (null != actionStorage) {
            actionStorageResult = actionStorage.getActionStorageResult(valueObject);
        }

        if (isNotEmpty(checkers)) {
            for (LifeCycleChecker<T> checker : checkers) {
                boolean filter = false;

                // Searching if is a checker to ignore
                if (isNotEmpty(forcedCheckers)) {
                    for (String idChecker : forcedCheckers) {
                        if (null != checker.getId() && idChecker.equalsIgnoreCase(checker.getId())) {
                            filter = true;
                            break;
                        }
                    }
                }

                if (!LifeCycleConstants.FALSE.equalsIgnoreCase(rtn)) {
                    rtn = checker.getTargetState();
                }

                String[] resultArray = checker.getResult(valueObject, actionStorageResult);
                String result = (null == resultArray || resultArray.length < 1) ? null : resultArray[0];
                String idPredicate = (null == resultArray || resultArray.length < 2) ? LifeCycleConstants.EMPTY_STRING : resultArray[1];

                if (null == result || LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
                    if (!filter) {
                        rtn = LifeCycleConstants.FALSE;
                        String message = "Failed checker : " + checker.getId() + ", predicate : " + idPredicate;
                        logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FAILED_CHECKER, message);
                    } else {
                        String message = "Forced checker : " + checker.getId() + ", predicate : " + idPredicate;
                        logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FORCED_CHECKER, message);
                    }
                }
            }
        }

        return rtn;
    }
}
