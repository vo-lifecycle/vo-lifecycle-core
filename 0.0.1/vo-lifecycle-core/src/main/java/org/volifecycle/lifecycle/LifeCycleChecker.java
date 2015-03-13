package org.volifecycle.lifecycle;

/**
 * Les "vérificateur" qui renvoient vrai si une condition est vérifiée ou pas
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T> objet métier
 */
public interface LifeCycleChecker<T> {
    /**
     * Retourner le résultat
     * 
     * @return "true" ou "false" par défaut
     */
    String getResult(T valueObject);

    /**
     * @return the id
     */
    String getId();

    /**
     * @return the description
     */
    String getDescription();
}
