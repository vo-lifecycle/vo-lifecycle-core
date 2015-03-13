package org.volifecycle.lifecycle;

import java.util.List;

/**
 * Transition interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T> valueObject type
 *
 */
public interface LifeCycleTransition<T> {
    /**
     * Verify all checker
     * 
     * @param valueObject
     * 
     * @return "true" if success or "false"
     */
    public String changeState(T valueObject);

    /**
     * Verify all checker with a forced id list
     * 
     * @param valueObject
     * @param forcedCheckers
     * @return
     */
    String changeState(T valueObject, List<String> forcedCheckers);

    /**
     * @return the type
     */
    String getType();

    /**
     * @return the description
     */
    String getDescription();

    /**
     * @return the target state
     */
    String getTarget();
}
