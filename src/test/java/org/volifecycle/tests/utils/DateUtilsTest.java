package org.volifecycle.tests.utils;

import static org.junit.Assert.assertEquals;
import static org.volifecycle.utils.DateUtils.calendarToString;
import static org.volifecycle.utils.DateUtils.getCurrentTime;
import static org.volifecycle.utils.DateUtils.stringToCalendar;

import java.text.ParseException;
import java.util.Calendar;
import java.util.GregorianCalendar;

import org.junit.Test;
import org.volifecycle.tests.AbstractTest;

/**
 * DateUtils method's tests
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class DateUtilsTest extends AbstractTest {
	/**
	 * Test current date
	 */
	@Test
	public final void testGetCurrentTime() {
		assertEquals(calendarToString(Calendar.getInstance()),
				calendarToString(getCurrentTime()));
	}

	/**
	 * Test convert string to calendar
	 */
	@Test
	public final void testCalendarToString() {
		Calendar d = new GregorianCalendar();
		d.set(Calendar.YEAR, 2014);
		d.set(Calendar.MONTH, Calendar.SEPTEMBER);
		d.set(Calendar.DAY_OF_MONTH, 5);

		assertEquals("05/09/2014", calendarToString(d));
	}

	/**
	 * Test convert calendar to string
	 */
	@Test
	public final void testStringToCalendar() {
		try {
			Calendar d = stringToCalendar("05/09/2014");
			assertEquals(2014, d.get(Calendar.YEAR));
			assertEquals(Calendar.SEPTEMBER, d.get(Calendar.MONTH));
			assertEquals(5, Calendar.DAY_OF_MONTH);
		} catch (ParseException e) {
			failWithException(e);
		}
	}
}
