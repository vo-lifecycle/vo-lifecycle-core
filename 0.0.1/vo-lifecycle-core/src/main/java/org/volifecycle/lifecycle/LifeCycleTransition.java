package org.volifecycle.lifecycle;

import java.util.List;

/**
 * Interface pour les transitions
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public interface LifeCycleTransition<T> {
    static final String AUTO = "auto";
    static final String MANUEL = "manuel";

    /**
     * Exécuter la liste des checker et changer l'état
     * 
     * @param valueObject
     * 
     * @return String "true" ou "false" par défaut
     */
    public String changeState(T valueObject);

    /**
     * Exécuter la liste des checker et changer l'état
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
