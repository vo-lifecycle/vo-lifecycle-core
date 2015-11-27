package org.volifecycle.utils;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * Utils static methods for dates
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 */
public class DateUtils {

    public static final String FORMAT_DATE = "dd/MM/yyyy";
    public static final String FORMAT_DATE_HOUR = "dd/MM/yyyy HH:mm:ss";
    public static final String FORMAT_DATE_HOUR_MINUTE = "dd/MM/yyyy HH:mm";
    public static final String FORMAT_Z = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    public static final String FORMAT_JOUR_DATE_HOUR = "EE dd/MM/yy HH:mm";

    /**
     * Convert dd/MM/yyyy string to calendar
     * 
     * @param str
     *            formated string
     * @param lenient
     * @return Calendar
     * @throws ParseException
     */
    public static final Calendar stringToCalendar(final String str, boolean lenient) throws ParseException {
        return stringToCalendar(str, FORMAT_DATE, lenient);
    }

    /**
     * Convert formated string to calendar
     * 
     * @param str
     *            formated string
     * @param format
     *            (ex : dd/MM/yyyy)
     * @param lenient
     * @return Calendar
     * @throws ParseException
     */
    public static final Calendar stringToCalendar(final String str, String format, boolean lenient) throws ParseException {
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        sdf.setLenient(lenient);
        Date date = sdf.parse(str);
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        return cal;
    }

    /**
     * Convert dd/MM/yyyy string to calendar
     * 
     * @param str
     *            formated string
     * @return Calendar
     * @throws ParseException
     */
    public static final Calendar stringToCalendar(final String str) throws ParseException {
        return stringToCalendar(str, true);
    }

    /**
     * Convert string to calendar
     * 
     * @param str
     *            formated string
     * @param format
     *            (ex : dd/MM/yyyy)
     * @return Calendar
     * @throws ParseException
     */
    public static final Calendar stringToCalendar(final String str, String format) throws ParseException {
        return stringToCalendar(str, format, true);
    }

    /**
     * Convert Calendar to String
     * 
     * @param Calendar
     *            c
     * @param format
     *            (ex : dd/MM/yyyy)
     * @return String
     */
    public static final String calendarToString(Calendar c, String format) {
        String rtn = null;

        if (c != null) {
            SimpleDateFormat sdf = new SimpleDateFormat(format, Locale.FRANCE);
            rtn = sdf.format(c.getTime());
        }

        return rtn;
    }

    /**
     * Convert Calendar to String (dd/MM/yyyy)
     * 
     * @param Calendar
     *            c
     * @return String
     */
    public static final String calendarToString(Calendar c) {
        return calendarToString(c, FORMAT_DATE);
    }

    /**
     * Get current date
     * 
     * @return Calendar
     */
    public static final Calendar getCurrentTime() {
        return (Calendar) Calendar.getInstance().clone();
    }

    /**
     * Static class
     */
    private DateUtils() {

    }
}
