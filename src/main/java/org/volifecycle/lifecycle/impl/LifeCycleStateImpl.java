package org.volifecycle.lifecycle.impl;

import java.util.Map;

import org.volifecycle.lifecycle.LifeCycleState;
import org.volifecycle.lifecycle.LifeCycleTransition;

/**
 * States implementation
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 *            value object type
 */
public class LifeCycleStateImpl<T> implements LifeCycleState<T> {
    /**
     * Transitions by id
     */
    protected Map<String, LifeCycleTransition<T>> transitionsById;

    protected String id;

    protected String description;

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, LifeCycleTransition<T>> getTransitionsById() {
        return transitionsById;
    }

    /**
     * @param transitionsById
     *            the transitionsById to set
     */
    public void setTransitionsById(Map<String, LifeCycleTransition<T>> transitionsById) {
        this.transitionsById = transitionsById;
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
}
