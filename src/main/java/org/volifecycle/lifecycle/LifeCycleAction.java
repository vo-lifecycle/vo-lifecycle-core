package org.volifecycle.lifecycle;

import java.util.Map;

/**
 * Action interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface LifeCycleAction<T> {
    /**
     * @return the id
     */
    String getId();

    /**
     * @return the description
     */
    String getDescription();

    /**
     * @return the additionnalInformations
     */
    Map<String, String> getAdditionnalInformations();

    /**
     * Return the result.
     * 
     * @param valueObject
     * @param actionStorage
     * @return "true" if success or "false"
     */
    String getResult(T valueObject, Map<String, Object> actionStorage);
}
