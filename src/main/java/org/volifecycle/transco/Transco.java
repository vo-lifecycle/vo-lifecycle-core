package org.volifecycle.transco;

import java.util.Map;

/**
 * Transco table interface.
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
     * @return String
     */
    String getQuietly(String key);

    /**
     * Return the key if not found
     * 
     * @param value
     * @return String
     */
    String getKey(String value);

    /**
     * Return the key if not found
     * 
     * @param value
     * @return String
     */
    String getKeyQuietly(String value);

    /**
     * Return the key if not found
     * 
     * @param key
     * @return String
     */
    String getOriginalQuietly(String key);

    /**
     * @return the k2v
     */
    Map<String, String> getK2v();

    /**
     * @return the v2k
     */
    Map<String, String> getV2k();
}
