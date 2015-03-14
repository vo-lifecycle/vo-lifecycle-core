package org.volifecycle.tests.transco;

import static org.junit.Assert.assertEquals;

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
	}
}
