package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;

import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleSynchronizedProcess;

/**
 * LifeCycleSynchronizedProcess implementation.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 *            : value object's type
 */
public class LifeCycleSynchronizedProcessImpl<T> implements LifeCycleSynchronizedProcess<T> {
    protected LifeCycleManagerImpl<T, LifeCycleAdapter<T>> manager;
    protected List<String> transitionIds;

    /**
     * {@inheritDoc}
     */
    @Override
    public LifeCycleManagerImpl<T, LifeCycleAdapter<T>> getManager() {
        return manager;
    }

    /**
     * @param manager
     *            the manager to set
     */
    public void setManager(LifeCycleManagerImpl<T, LifeCycleAdapter<T>> manager) {
        this.manager = manager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getTransitionIds() {
        return transitionIds;
    }

    /**
     * @param transitionIds
     *            the transitionIds to set
     */
    public void setTransitionIds(List<String> transitionIds) {
        this.transitionIds = transitionIds;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String process(T valueObject) throws IllegalStateException {
        String rtn = Boolean.FALSE.toString();
        if (isNotEmpty(transitionIds)) {
            for (String transitionId : transitionIds) {
                rtn = manager.runTransition(transitionId, valueObject);
                if (Boolean.FALSE.toString().equalsIgnoreCase(rtn)) {
                    break;
                }
            }
        }
        return rtn;
    }
}
