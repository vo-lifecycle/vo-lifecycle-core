package org.volifecycle.transco.impl;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.volifecycle.transco.Transco;

/**
 * Transco table implementation.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class TranscoImpl implements Transco {
    protected Map<String, String> k2v;
    protected Map<String, String> v2k;
    protected Map<String, String> original;

    /**
     * Constructor.
     */
    public TranscoImpl() {
        k2v = new HashMap<String, String>();
        v2k = new HashMap<String, String>();
        original = new HashMap<String, String>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return k2v.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return k2v.isEmpty();
    }

    /**
     * Case insensitive formater
     * 
     * @param key
     * @return String
     */
    private Object getFormatedKey(Object key) {
        return (!(key instanceof String) || null == key) ? null : ((String) key).toUpperCase();
    }

    /**
     * Case insensitive formater
     * 
     * @param key
     * @return String
     */
    private String getFormatedKey(String key) {
        return key = (null == key) ? key : key.toUpperCase();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsKey(Object key) {
        return k2v.containsKey(getFormatedKey(key));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsValue(Object value) {
        return v2k.containsKey(getFormatedKey(value));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String get(Object key) {
        return k2v.get(getFormatedKey(key));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getKey(String value) {
        return v2k.get(getFormatedKey(value));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getQuietly(String key) {
        if (!containsKey(key)) {
            return getFormatedKey(key);
        } else {
            return k2v.get(getFormatedKey(key));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getKeyQuietly(String value) {
        value = getFormatedKey(value);
        if (!v2k.containsKey(value)) {
            return value;
        } else {
            return v2k.get(value);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String put(String key, String value) {
        v2k.put(getFormatedKey(value), getFormatedKey(key));
        original.put(getFormatedKey(key), value);
        return k2v.put(getFormatedKey(key), getFormatedKey(value));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String remove(Object key) {
        String value = get(key);
        v2k.remove(getFormatedKey(value));
        original.remove(getFormatedKey(key));
        return k2v.remove(getFormatedKey(key));
    }

    /**
     * Return a map with formated key
     * 
     * @param m
     * @return Map<String, String>
     */
    private Map<String, String> getFormatedMap(Map<? extends String, ? extends String> m) {
        return getFormatedMap(m, true);
    }

    /**
     * Return a map with formated key
     * 
     * @param m
     * @param formatValue
     * @return
     */
    private Map<String, String> getFormatedMap(Map<? extends String, ? extends String> m, boolean formateValue) {
        Map<String, String> m2 = null;

        if (null != m) {
            m2 = new HashMap<String, String>();
            for (Entry<? extends String, ? extends String> entry : m.entrySet()) {
                String key = entry.getKey();
                String value = entry.getValue();
                m2.put(getFormatedKey(key), (formateValue) ? getFormatedKey(value) : value);
            }
        }

        return m2;
    }

    /**
     * Return the reversed map
     * 
     * @param m
     * @return
     */
    private Map<String, String> getReversedMap(Map<? extends String, ? extends String> m) {
        Map<String, String> m2 = null;

        if (null != m) {
            m2 = new HashMap<String, String>();
            for (Entry<? extends String, ? extends String> entry : m.entrySet()) {
                String key = entry.getKey();
                String value = entry.getValue();
                m2.put(getFormatedKey(value), getFormatedKey(key));
            }
        }

        return m2;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putAll(Map<? extends String, ? extends String> m) {
        k2v.putAll(getFormatedMap(m));
        original.putAll(getFormatedMap(m, false));
        this.v2k = getReversedMap(k2v);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        k2v.clear();
        v2k.clear();
        original.clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> keySet() {
        return k2v.keySet();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<String> values() {
        return k2v.values();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Entry<String, String>> entrySet() {
        return k2v.entrySet();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getMap() {
        return k2v;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setMap(Map<String, String> map) {
        this.k2v = getFormatedMap(map);
        this.original = getFormatedMap(map, false);
        this.v2k = getReversedMap(k2v);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getOriginalQuietly(String key) {
        if (!containsKey(key)) {
            return getFormatedKey(key);
        } else {
            return original.get(getFormatedKey(key));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getK2v() {
        return k2v;
    }

    /**
     * @param k2v the k2v to set
     */
    public void setK2v(Map<String, String> k2v) {
        this.k2v = k2v;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, String> getV2k() {
        return v2k;
    }

    /**
     * @param v2k the v2k to set
     */
    public void setV2k(Map<String, String> v2k) {
        this.v2k = v2k;
    }
}
