package org.volifecycle.tests.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.volifecycle.tests.PojoA;
import org.volifecycle.tests.PojoB;
import org.volifecycle.utils.BeanMapperFactory;

/**
 * Test of BeanMapperFactory class.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class BeanMapperFactoryTest {

  @Test
  public void testMapNewObjectNominal() {
    // Given
    PojoA objA = new PojoA().setP1("1").setP2("2");

    // When
    PojoB objB = BeanMapperFactory.getInstance().map(objA, PojoB.class);

    // Then
    assertNotNull(objB);
    assertNotNull(objB.getP1());
    assertEquals(objA.getP1(), objB.getP1());
    assertEquals("1", objB.getP1());
    assertNull(objB.getP3());
  }

  @Test
  public void testMapExistingInstanceNominal() {
    // Given
    PojoA objA = new PojoA().setP1("1").setP2("2");
    PojoB objB = new PojoB().setP1("11").setP3("3");

    // When
    BeanMapperFactory.getInstance().map(objA, objB);

    // Then
    assertNotNull(objB);
    assertNotNull(objB.getP1());
    assertEquals(objA.getP1(), objB.getP1());
    assertEquals("1", objB.getP1());
    assertNotNull(objB.getP3());
    assertEquals("3", objB.getP3());
  }
}
