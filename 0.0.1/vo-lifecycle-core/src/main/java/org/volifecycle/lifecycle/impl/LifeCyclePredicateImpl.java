package org.volifecycle.lifecycle.impl;

import org.volifecycle.lifecycle.LifeCyclePredicate;

/**
 * Abstract implementation of LifeCyclePredicate.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 *            value object type
 */
public abstract class LifeCyclePredicateImpl<T> implements LifeCyclePredicate<T> {
    /**
     * Id of predicate.
     */
    private String id;

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
}
