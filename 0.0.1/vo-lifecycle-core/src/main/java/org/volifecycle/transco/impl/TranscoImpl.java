package org.volifecycle.transco.impl;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.volifecycle.transco.Transco;

/**
 * Transco table implementation
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class TranscoImpl implements Transco {
	protected Map<String, String> k2v;
	protected Map<String, String> v2k;

	/**
	 * Constructor
	 */
	public TranscoImpl() {
		k2v = new HashMap<String, String>();
		v2k = new HashMap<String, String>();
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
		return (!(key instanceof String) || null == key) ? null
				: ((String) key).toUpperCase();
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
		return k2v.containsValue(value);
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
		return k2v.put(getFormatedKey(key), getFormatedKey(value));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String remove(Object key) {
		String value = get(key);
		v2k.remove(getFormatedKey(value));
		return k2v.remove(getFormatedKey(key));
	}

	/**
	 * Return a map with formated key
	 * 
	 * @param m
	 * @return
	 */
	private Map<String, String> getFormatedMap(
			Map<? extends String, ? extends String> m) {
		Map<String, String> m2 = null;

		if (null != m) {
			m2 = new HashMap<String, String>();
			for (Entry<? extends String, ? extends String> entry : m.entrySet()) {
				String key = entry.getKey();
				String value = entry.getValue();
				m2.put(getFormatedKey(key), getFormatedKey(value));
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
	private Map<String, String> getReversedMap(
			Map<? extends String, ? extends String> m) {
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
		this.v2k = getReversedMap(k2v);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void clear() {
		k2v.clear();
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
		this.v2k = getReversedMap(k2v);
	}
}
