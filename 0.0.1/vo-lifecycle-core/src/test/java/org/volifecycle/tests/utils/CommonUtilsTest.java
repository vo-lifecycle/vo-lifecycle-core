package org.volifecycle.tests.utils;

import static org.apache.commons.lang3.StringUtils.isEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.volifecycle.tests.AbstractTest;
import org.volifecycle.utils.CommonUtils;

/**
 * Test for common utils class.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class CommonUtilsTest extends AbstractTest {
    /**
     * implode test nominal
     */
    @Test
    public final void testImplodeNominal() {
        List<String> data = new ArrayList<String>();
        data.add("d1");
        data.add("d2");

        String result = CommonUtils.implode(",", data);
        assertEquals("d1,d2", result);
    }

    /**
     * implode test with only one value
     */
    @Test
    public final void testImplodeWithOnlyOneValue() {
        List<String> data = new ArrayList<String>();
        data.add("d1");

        String result = CommonUtils.implode(",", data);
        assertEquals("d1", result);
    }

    /**
     * implode test null
     */
    @Test
    public final void testImplodeNull() {
        String result = CommonUtils.implode(",", null);
        assertTrue(isEmpty(result));
    }
}
