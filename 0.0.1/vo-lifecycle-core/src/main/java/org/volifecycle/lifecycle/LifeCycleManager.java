package org.volifecycle.lifecycle;

import java.util.List;

/**
 * Interface pour les transitions
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T> ValueObject Type
 * @param <A> Adapter Type
 */
public interface LifeCycleManager<T, A extends LifeCycleAdapter<T>> {
    /**
     * Exécuter une transition à partir de son id et de l'état d'un DC
     * 
     * @param transitionId
     * @param valueObject
     * @return String "true" ou "false"
     */
    String runTransition(String transitionId, T valueObject);

    /**
     * @return the description
     */
    String getDescription();

    /**
     * Exécuter une transition à partir de son id et de l'état d'un DC avec une
     * liste de checker
     * 
     * @param idTransition
     * @param valueObject
     * @param forcedCheckers
     * @return
     */
    String runTransition(String idTransition, T valueObject, List<String> forcedCheckers);
}
