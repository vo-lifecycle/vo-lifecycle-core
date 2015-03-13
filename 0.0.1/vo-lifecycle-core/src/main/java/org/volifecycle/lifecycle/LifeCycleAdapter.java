package org.volifecycle.lifecycle;

/**
 * Interface permettant de retourner l'état d'un objet métier
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T> objet métier
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
