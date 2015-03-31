package org.volifecycle.event.impl;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.volifecycle.utils.DateUtils.FORMAT_DATE_HOUR;
import static org.volifecycle.utils.DateUtils.calendarToString;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.volifecycle.common.AbstractLifeCycle;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.DiffDetector;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.DiffEvent;
import org.volifecycle.event.vo.DiffProperty;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * 
 * @author neumann
 * 
 * @param <T>
 */
public class DiffDetectorImpl<T, A extends LifeCycleAdapter<T>> extends AbstractLifeCycle<T> implements DiffDetector<T, A> {
    /**
     * Event manager
     */
    private EventManager evtManager;

    /**
     * Adapter
     */
    private A adapter;

    private List<String> propertyFilters;
    private List<String> classFilters;

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean compare(T vo1, T vo2) {
        boolean rtn = false;

        EventManager evtManager = getEvtManager();

        if (null == evtManager) {
            evtManager = new Log4jEventManagerImpl();
        }

        if (null == vo1 || null == vo2) {
            throw new IllegalStateException("Value objects must be not null !");
        }

        DiffEvent event = new DiffEvent();
        String details = "There are some changes...";
        setCustomEvent(event, vo1, adapter, LifeCycleConstants.EVENT_TYPE_DIFF_VO, details);
        List<DiffProperty> diffs = logDiffs(vo1, vo1, vo2, new ArrayList<DiffProperty>(), null);
        event.setDiffProperties(diffs);

        if (isNotEmpty(diffs)) {
            evtManager.logEvent(event);
        }

        return rtn;
    }

    /**
     * Convert objectToString
     * 
     * @param o
     * @return String
     */
    public String object2string(Object o) {
        Class<?> clazz;
        if (null == o) {
            return "null";
        } else if (o instanceof Calendar) {
            return calendarToString((Calendar) o, FORMAT_DATE_HOUR);
        } else if (null != (clazz = getPrimitifType(o))) {
            return String.valueOf(clazz.cast(o));
        } else {
            return o.toString();
        }
    }

    /**
     * Logs diff
     * 
     * @param original
     * @param vo1
     * @param vo2
     */
    public List<DiffProperty> logDiffs(T original, Object vo1, Object vo2, List<DiffProperty> diffs, String parent) {
        if (null == vo1 || null == vo2) {
            if (null != vo1 || null != vo2) {
                diffs.add(createDiffProperty((null == parent) ? vo2.getClass().getSimpleName() : parent, object2string(vo1), object2string(vo2), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
            }
            return diffs;
        }

        Class<?> clazz = vo1.getClass();
        if (isEmpty(classFilters) || !classFilters.contains(clazz.getName())) {
            return diffs;
        }

        List<Method> getters = getCommonGetter(vo1, vo2);
        if (isNotEmpty(getters)) {
            Object o1, o2;

            for (Method getter : getters) {
                // Camel case
                String property = getter.getName().replaceAll("^get", "");
                property = property.substring(0, 1).toLowerCase() + property.substring(1);

                if (isEmpty(propertyFilters) || !propertyFilters.contains(property)) {
                    continue;
                }

                try {
                    o1 = getter.invoke(vo1);
                    o2 = getter.invoke(vo2);
                } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                    String message = "Reflexion error : " + e.getMessage();
                    logCustomEvent(original, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_REFLEXION_ERROR, message);
                    continue;
                }

                // Not implemented type
                if (null != getNotImplementedType(o1) || null != getNotImplementedType(o2)) {
                    continue;
                } else if (o1 instanceof List && o2 instanceof List) {
                    List<?> l1 = (List<?>) o1;
                    List<?> l2 = (List<?>) o2;

                    if (isNotEmpty(l1) && isNotEmpty(l2) && l1.size() == l2.size()) {
                        // Recursive
                        for (Integer i = 0; i < l1.size(); i++) {
                            logDiffs(original, l1.get(i), l2.get(i), diffs, property);
                        }
                    } else if (isNotEmpty(l1) && isNotEmpty(l2)) {
                        diffs.add(createDiffProperty(property, String.valueOf(l1.size()), String.valueOf(l2.size()), parent, LifeCycleConstants.DIFF_TYPE_SIZE));

                        List<?> lmin = (l1.size() >= l2.size()) ? l2 : l1;
                        List<?> lmax = (l1.size() >= l2.size()) ? l1 : l2;

                        // Adding value
                        for (Integer i = lmin.size(); i < lmax.size(); i++) {
                            logDiffs(original, null, lmax.get(i), diffs, property);
                        }
                    } else if (isNotEmpty(l1)) {
                        diffs.add(createDiffProperty(property, String.valueOf(l1.size()), "null", parent, LifeCycleConstants.DIFF_TYPE_SIZE));

                        // Adding value
                        for (Integer i = 0; i < l1.size(); i++) {
                            logDiffs(original, l1.get(i), null, diffs, property);
                        }
                    } else {
                        diffs.add(createDiffProperty(property, "null", String.valueOf(l2.size()), parent, LifeCycleConstants.DIFF_TYPE_SIZE));

                        // Adding value
                        for (Integer i = 0; i < l2.size(); i++) {
                            logDiffs(original, null, l2.get(i), diffs, property);
                        }
                    }
                } else if (null != (clazz = getPrimitifType(o1))) {
                    if (!clazz.cast(o1).equals(clazz.cast(o2))) {
                        diffs.add(createDiffProperty(property, String.valueOf(clazz.cast(o1)), String.valueOf(clazz.cast(o2)), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
                    }
                } else if (o1 instanceof Calendar && o2 instanceof Calendar) {
                    Calendar c1 = (Calendar) o1;
                    Calendar c2 = (Calendar) o2;
                    if (!c1.equals(c2)) {
                        diffs.add(createDiffProperty(property, calendarToString(c1, FORMAT_DATE_HOUR), calendarToString(c2, FORMAT_DATE_HOUR), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
                    }
                } else {
                    // Recursive
                    logDiffs(original, o1, o2, diffs, property);
                }
            }
        }

        return diffs;
    }

    /**
     * Create diff property
     * 
     * @param name
     * @param before
     * @param after
     * @param parent
     * @param type
     * @return DiffProperty
     */
    public DiffProperty createDiffProperty(String name, String before, String after, String parent, String type) {
        DiffProperty diff = new DiffProperty();
        diff.setPropertyName(name);
        diff.setBeforeValue(before);
        diff.setAfterValue(after);
        diff.setParentPropertyName(parent);
        diff.setType(type);
        return diff;
    }

    /**
     * Get list of primitif type
     * 
     * @return List<Class<?>>
     */
    public List<Class<?>> getPrimitifTypes() {
        List<Class<?>> rtn = new ArrayList<Class<?>>();
        rtn.add(Number.class);
        rtn.add(String.class);
        rtn.add(Boolean.class);
        rtn.add(Integer.class);
        rtn.add(Long.class);
        rtn.add(Double.class);
        rtn.add(Short.class);
        rtn.add(Float.class);
        rtn.add(BigDecimal.class);
        return rtn;
    }

    /**
     * Get list of not allowed type
     * 
     * @return List<Class<?>>
     */
    public List<Class<?>> getNotImplementedTypes() {
        List<Class<?>> rtn = new ArrayList<Class<?>>();
        rtn.add(Map.class);
        return rtn;
    }

    /**
     * Get primitif type
     * 
     * @param o
     * @return Class<?>
     */
    public Class<?> getNotImplementedType(Object o) {
        for (Class<?> c : getNotImplementedTypes()) {
            if (c.isInstance(o)) {
                return c;
            }
        }

        return null;
    }

    /**
     * Get primitif type
     * 
     * @param o
     * @return Class<?>
     */
    public Class<?> getPrimitifType(Object o) {
        for (Class<?> c : getPrimitifTypes()) {
            if (c.isInstance(o)) {
                return c;
            }
        }

        return null;
    }

    /**
     * Get common methods of two value object
     * 
     * @param vo1
     * @param vo2
     * @return List<Method>
     */
    private List<Method> getCommonGetter(Object vo1, Object vo2) {
        Method[] methods = vo1.getClass().getMethods();
        Method[] methods2 = vo2.getClass().getMethods();
        List<Method> rtn = new ArrayList<Method>();

        if (null != methods && null != methods2 && methods.length > 0 && methods2.length > 0) {
            for (Method m1 : methods) {
                for (Method m2 : methods) {
                    if (m1.getName().startsWith("get") && !m1.getName().equals("getClass") && m1.getName().equals(m2.getName())) {
                        rtn.add(m1);
                    }
                }
            }
        }

        return rtn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventManager getEvtManager() {
        return evtManager;
    }

    /**
     * @param evtManager the evtManager to set
     */
    public void setEvtManager(EventManager evtManager) {
        this.evtManager = evtManager;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public A getAdapter() {
        return adapter;
    }

    /**
     * @param adapter the adapter to set
     */
    public void setAdapter(A adapter) {
        this.adapter = adapter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getPropertyFilters() {
        return propertyFilters;
    }

    /**
     * @param propertyFilters the propertyFilters to set
     */
    public void setPropertyFilters(List<String> propertyFilters) {
        this.propertyFilters = propertyFilters;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<String> getClassFilters() {
        return classFilters;
    }

    /**
     * @param classFilters the classFilters to set
     */
    public void setClassFilters(List<String> classFilters) {
        this.classFilters = classFilters;
    }
}
