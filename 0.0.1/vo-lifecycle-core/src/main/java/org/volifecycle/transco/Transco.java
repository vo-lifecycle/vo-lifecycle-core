package org.volifecycle.transco;

import java.util.Map;

/**
 * Interface pour les tables de transco (potentiellement initialisés dans une
 * conf Spring ou depuis une bdd)
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public interface Transco extends Map<String, String> {
    /**
     * Retourner la map (définie dans une conf spring)
     * 
     * @return Map<String, String>
     */
    Map<String, String> getMap();

    /**
     * Définir la map dans une conf Spring
     * 
     * @param map
     */
    void setMap(Map<String, String> map);

    /**
     * Retourne la clef si valeur non trouvée
     * 
     * @param key
     * @return
     */
    String getQuietly(String key);

    /**
     * Retourne la clef pour une valeur donnée
     * 
     * @param value
     * @return
     */
    String getKey(String value);

    /**
     * Retourne la valeur si clef non trouvée
     * 
     * @param value
     * @return
     */
    String getKeyQuietly(String value);
}

