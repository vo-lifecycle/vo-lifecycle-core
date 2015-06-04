package org.volifecycle.lifecycle;

import java.util.Map;

/**
 * Predicate interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 *            value object type
 */
public interface LifeCyclePredicate<T> {
    /**
     * Return the result
     * 
     * @param valueObject
     * @return "true" if success or "false"
     */
    String getResult(T valueObject);

    /**
     * Return the result
     * 
     * @param valueObject
     * @param actionStorage
     * @return "true" if success or "false"
     */
    String getResult(T valueObject, Map<String, Object> actionStorage);
}
