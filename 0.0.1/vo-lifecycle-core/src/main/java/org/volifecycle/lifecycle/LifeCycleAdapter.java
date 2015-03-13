package org.volifecycle.lifecycle;

/**
 * Value object's adapter interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T> value object type
 */
public interface LifeCycleAdapter<T> {
    /**
     * Retourner la valeur de la propriété de l'état
     * 
     * @param valueObject
     * @return String
     */
    String getState(T valueObject);

    /**
     * Changer d'état
     * 
     * @param valueObject
     * @param targetState
     */
    void setState(T valueObject, String targetState);
}
