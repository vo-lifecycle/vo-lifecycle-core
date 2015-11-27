package org.volifecycle.lifecycle;

/**
 * Value object's adapter interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface LifeCycleAdapter<T> {
    /**
     * Return state of value object.
     * 
     * @param valueObject
     * @return state
     */
    String getState(T valueObject);

    /**
     * Return id of value object.
     * 
     * @param valueObject
     * @return id
     */
    String getId(T valueObject);

    /**
     * Return type of value object.
     * 
     * @param valueObject
     * @return type
     */
    String getType(T valueObject);

    /**
     * Return the actor who is responsible.
     * 
     * @param valueObject
     * @return actor
     */
    String getActor(T valueObject);

    /**
     * Setting a new state.
     * 
     * @param valueObject
     * @param targetState
     */
    void setState(T valueObject, String targetState);
}
