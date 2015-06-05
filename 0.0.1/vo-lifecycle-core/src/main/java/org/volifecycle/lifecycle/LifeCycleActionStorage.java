package org.volifecycle.lifecycle;

import java.util.Map;

/**
 * Action's storage.
 * 
 * @author Idriss Neumann <idriss.neumann@capgemini.com>
 *
 * @param <T>
 *            value object's type
 */
public interface LifeCycleActionStorage<T> {
    /**
     * Getting action storage from value object.
     * 
     * @param valueObject
     * @return Map<String, Object>
     */
    public Map<String, Object> getActionStorageResult(T valueObject);
}
