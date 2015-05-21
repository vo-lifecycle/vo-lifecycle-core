package org.volifecycle.tests.transco;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.volifecycle.tests.AbstractTest;
import org.volifecycle.transco.Transco;
import org.volifecycle.transco.impl.TranscoImpl;

/**
 * TranscoImpl tests
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class TranscoImplTest extends AbstractTest {
    Transco t;
    Transco t2;

    /**
     * Init datas
     */
    @Before
    public final void initData() {
        t = new TranscoImpl();
    }

    /**
     * Test searching value by key
     */
    @Test
    public final void testGetNominal() {
        String key = "k";
        String value = "v";

        t.put(key, value);
        assertEquals(value.toUpperCase(), t.get(key));

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);
        t2 = new TranscoImpl();
        t2.setMap(m);
        assertEquals(value.toUpperCase(), t2.get(key));
    }

    /**
     * Test searching key by value
     */
    @Test
    public final void testGetKeyNominal() {
        String key = "k";
        String value = "v";

        t.put(key, value);
        assertEquals(key.toUpperCase(), t.getKey(value));

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);
        t2 = new TranscoImpl();
        t2.setMap(m);
        assertEquals(key.toUpperCase(), t2.getKey(value));
    }

    /**
     * Test searching value by key with getQuietly
     */
    @Test
    public final void testGetQuietlyNominal() {
        String key = "k";
        String value = "v";

        t.put(key, value);
        assertEquals(value.toUpperCase(), t.getQuietly(key));

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);
        t2 = new TranscoImpl();
        t2.setMap(m);
        assertEquals(value.toUpperCase(), t2.getQuietly(key));
    }

    /**
     * Test searching key by value with getQuietly
     */
    @Test
    public final void testGetKeyQuietlyNominal() {
        String key = "k";
        String value = "v";

        t.put(key, value);
        assertEquals(key.toUpperCase(), t.getKeyQuietly(value));

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);
        t2 = new TranscoImpl();
        t2.setMap(m);
        assertEquals(key.toUpperCase(), t2.getKeyQuietly(value));
    }

    /**
     * Test searching value by key quietly
     */
    @Test
    public final void testGetQuietlyNotExists() {
        String key = "k";
        assertEquals(key.toUpperCase(), t.getQuietly(key));
    }

    /**
     * Test searching key quietly
     */
    @Test
    public final void testGetKeyQuietlyNotExists() {
        String value = "v";
        assertEquals(value.toUpperCase(), t.getKeyQuietly(value));
    }

    /**
     * Test searching value by key with getQuietly
     */
    @Test
    public final void testGetOriginalQuietlyNominal() {
        String key = "k";
        String value = "v";

        t.put(key, value);
        assertEquals(value.toUpperCase(), t.getQuietly(key));

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);
        t2 = new TranscoImpl();
        t2.setMap(m);
        assertEquals(value, t2.getOriginalQuietly(key));
    }

    /**
     * Test searching value by key quietly
     */
    @Test
    public final void testGetOriginalQuietlyNotExists() {
        String key = "k";
        assertEquals(key.toUpperCase(), t.getOriginalQuietly(key));
    }

    /**
     * Test function putAll
     */
    @Test
    public final void testPutAll() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        assertEquals(value.toUpperCase(), t.get(key));
        assertEquals(key.toUpperCase(), t.getKey(value));
    }

    /**
     * Test function clear
     */
    @Test
    public final void testClear() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        assertFalse(t.isEmpty());
        t.clear();
        assertNull(t.get(key));
        assertNull(t.getKey(value));
        assertTrue(t.isEmpty());
    }

    /**
     * Test function remove
     */
    @Test
    public final void testRemove() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        t.remove(key);
        assertNull(t.get(key));
        assertNull(t.getKey(value));
    }

    /**
     * Test function containValue
     */
    @Test
    public final void testContainsValue() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        assertTrue(t.containsValue(value));
    }

    /**
     * Test function getMap
     */
    @Test
    public final void testGetMap() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        Map<String, String> result = t.getMap();
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.containsKey(key.toUpperCase()));
    }

    /**
     * Test function size
     */
    @Test
    public final void testSize() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        assertEquals(1, t.size());
    }

    /**
     * Test function values
     */
    @Test
    public final void testValues() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        Collection<String> result = t.values();
        assertNotNull(result);
        assertEquals(1, result.size());
    }

    /**
     * Test function keySet
     */
    @Test
    public final void testKeySet() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        Set<String> result = t.keySet();
        assertNotNull(result);
        assertEquals(1, result.size());
    }

    /**
     * Test function keySet
     */
    @Test
    public final void testEntrySet() {
        String key = "k";
        String value = "v";

        Map<String, String> m = new HashMap<String, String>();
        m.put(key, value);

        t.putAll(m);
        Set<Entry<String, String>> result = t.entrySet();
        assertNotNull(result);
        assertEquals(1, result.size());
    }
}
