package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;
import java.util.Map;

import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.lifecycle.LifeCycleChecker;
import org.volifecycle.lifecycle.LifeCyclePredicate;

/**
 * Implementation of checker
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject
 */
public abstract class LifeCycleCheckerImpl<T> implements LifeCycleChecker<T> {
    /**
     * Id which is used for forced the result of this checker
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
     * List predicates wich are executed by and
     */
    protected List<LifeCyclePredicate<T>> predicates;

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
    public List<LifeCyclePredicate<T>> getPredicates() {
        return predicates;
    }

    /**
     * @param predicates
     *            the predicates to set
     */
    public void setPredicates(List<LifeCyclePredicate<T>> predicates) {
        this.predicates = predicates;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject) {
        return getResult(valueObject, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getResult(T valueObject, Map<String, Object> actionStorage) {
        String result = LifeCycleConstants.TRUE;
        if (isNotEmpty(predicates)) {
            for (LifeCyclePredicate<T> predicate : predicates) {
                result = predicate.getResult(valueObject, actionStorage);
                if (LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
                    break;
                }
            }
        }
        return result;
    }
}
