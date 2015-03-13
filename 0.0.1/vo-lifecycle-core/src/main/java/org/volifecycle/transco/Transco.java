package org.volifecycle.transco;

import java.util.Map;

/**
 * Transco table interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public interface Transco extends Map<String, String> {
    /**
     * Return the initial map definition
     * 
     * @return Map<String, String>
     */
    Map<String, String> getMap();

    /**
     * Set the initial map definition
     * 
     * @param map
     */
    void setMap(Map<String, String> map);

    /**
     * Return the key if not found
     * 
     * @param key
     * @return
     */
    String getQuietly(String key);

    /**
     * Return the key if not found
     * 
     * @param value
     * @return
     */
    String getKey(String value);

    /**
     * Return the key if not found
     * 
     * @param value
     * @return
     */
    String getKeyQuietly(String value);
}

