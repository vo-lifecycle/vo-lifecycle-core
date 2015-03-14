package org.volifecycle.tests.transco;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

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
}
