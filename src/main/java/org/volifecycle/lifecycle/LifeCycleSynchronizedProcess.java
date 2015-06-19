package org.volifecycle.lifecycle;

import java.util.List;

import org.volifecycle.lifecycle.impl.LifeCycleManagerImpl;

/**
 * LifeCycle process is used to launch multiple transitions.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 *            : value object's type
 */
public interface LifeCycleSynchronizedProcess<T> {
    /**
     * @return the manager
     */
    LifeCycleManagerImpl<T, LifeCycleAdapter<T>> getManager();

    /**
     * @return the transitionIds
     */
    List<String> getTransitionIds();

    /**
     * Launching a list of transition on a value object
     * 
     * @param valueObject
     * @return the last state if success or "false"
     */
    String process(T valueObject) throws IllegalStateException;
}
