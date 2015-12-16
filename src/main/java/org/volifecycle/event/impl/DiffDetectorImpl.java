package org.volifecycle.event.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.volifecycle.event.EventBuilder.build;
import static org.volifecycle.utils.DateUtils.FORMAT_DATE_HOUR;
import static org.volifecycle.utils.DateUtils.calendarToString;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.ClassListener;
import org.volifecycle.event.DiffDetector;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.DiffEvent;
import org.volifecycle.event.vo.DiffProperty;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * DiffDetector implementation.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 */
public class DiffDetectorImpl<T, A extends LifeCycleAdapter<T>> implements DiffDetector<T, A> {
    /**
     * Event manager
     */
    private EventManager evtManager;

    /**
     * Adapter
     */
    private A adapter;

    /**
     * List of classes to watch
     */
    private List<ClassListener> classListeners;

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean compare(T vo1, T vo2, String parentId, String parentType, Map<String, String> additionalInformations) {
        boolean rtn = false;

        EventManager evtManager = getEvtManager();

        if (null == evtManager) {
            evtManager = new LogEventManagerImpl();
        }

        DiffEvent event = new DiffEvent();
        String details = "There are some changes...";
        build(event, vo1, adapter, LifeCycleConstants.EVENT_TYPE_DIFF_VO, details, additionalInformations, null, null);
        List<DiffProperty> diffs = logDiffs(vo2, vo1, vo2, new ArrayList<DiffProperty>(), null);
        event.setDiffProperties(diffs);
        event.setParentId(parentId);
        event.setParentType(parentType);

        if (isNotEmpty(diffs)) {
            evtManager.logEvent(event);
        }

        return rtn;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean compare(T vo1, T vo2, String parentId, String parentType) {
        return compare(vo1, vo2, parentId, parentType, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean compare(T vo1, T vo2) {
        return compare(vo1, vo2, null, null);
    }

    /**
     * Convert objectToString.
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
        } else if (null != (clazz = getSimpleType(o))) {
            return String.valueOf(clazz.cast(o));
        } else {
            return o.toString();
        }
    }

    /**
     * Get listener for object class.
     * 
     * @param o
     * @return ClassListener
     */
    private ClassListener getListener(Object o) {
        if (null != o) {
            if (isNotEmpty(classListeners)) {
                for (ClassListener c : classListeners) {
                    if (c.getClassName().equalsIgnoreCase(o.getClass().getName())) {
                        return c;
                    }
                }
            }
        }

        return null;
    }

    /**
     * Testing if a class exist in the listener list.
     * 
     * @param o
     * @return boolean
     */
    private boolean isClassListened(Object o) {
        return null != getListener(o);
    }

    /**
     * Testing if a property exists in the listener list.
     * 
     * @param o
     * @param property
     * @return boolean
     */
    private boolean isPropertyListened(Object o, String property) {
        ClassListener l = getListener(o);
        return null != l && isNotEmpty(l.getProperties()) && l.getProperties().contains(property);
    }

    /**
     * Logs when one of two vo is NULL.
     * 
     * @param original
     * @param o
     * @param diffs
     * @param parent
     * @param isBefore
     * @return List<DiffProperty>
     */
    public List<DiffProperty> logDiffsWhereNull(T original, Object o, List<DiffProperty> diffs, String parent, boolean isBefore) {
        if (o instanceof Calendar || null != getSimpleType(o)) {
            diffs.add(createDiffProperty((null == parent) ? o.getClass().getSimpleName() : parent, object2string((!isBefore) ? null : o), object2string((isBefore) ? null : o), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
        } else {
            if (!isClassListened(o)) {
                return diffs;
            }

            if (null == evtManager) {
                evtManager = new LogEventManagerImpl();
            }

            List<Method> getters = getCommonGetter(o, o);
            if (isNotEmpty(getters)) {
                for (Method getter : getters) {
                    Object so;
                    String property = getPropertyFromGetter(getter);

                    if (!isPropertyListened(o, property)) {
                        continue;
                    }

                    try {
                        so = getter.invoke(o);
                    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                        String message = "Reflexion error : " + e.getMessage();
                        Event evt = build(original, adapter, LifeCycleConstants.EVENT_TYPE_REFLEXION_ERROR, message, null, null, null);
                        evtManager.logEvent(evt);
                        continue;
                    }

                    if (null != getNotImplementedType(so)) {
                        continue;
                    } else if (null != getSimpleType(so)) {
                        diffs.add(createDiffProperty(property, object2string((!isBefore) ? null : so), object2string((isBefore) ? null : so), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
                    } else {
                        diffs = logDiffsWhereNull(original, so, diffs, property, isBefore);
                    }
                }
            }
        }

        return diffs;
    }

    /**
     * Get property from getter.
     * 
     * @param getter
     * @return String
     */
    private String getPropertyFromGetter(Method getter) {
        // Camel case
        String property = getter.getName().replaceAll("^get", "");
        property = property.substring(0, 1).toLowerCase() + property.substring(1);
        return property;
    }

    /**
     * Logs differences.
     * 
     * @param original
     * @param vo1
     * @param vo2
     */
    public List<DiffProperty> logDiffs(T original, Object vo1, Object vo2, List<DiffProperty> diffs, String parent) {
        // When one of two value object is null
        if (null == vo1 || null == vo2) {
            if (null != vo1) {
                diffs = logDiffsWhereNull(original, vo1, diffs, parent, true);
            } else if (null != vo2) {
                diffs = logDiffsWhereNull(original, vo2, diffs, parent, false);
            }
            return diffs;
        }

        Class<?> clazz = vo1.getClass();
        if (!isClassListened(vo1)) {
            return diffs;
        }

        if (null == evtManager) {
            evtManager = new LogEventManagerImpl();
        }

        List<Method> getters = getCommonGetter(vo1, vo2);
        if (isNotEmpty(getters)) {
            Object o1, o2;

            for (Method getter : getters) {
                String property = getPropertyFromGetter(getter);

                if (!isPropertyListened(vo1, property)) {
                    continue;
                }

                try {
                    o1 = getter.invoke(vo1);
                    o2 = getter.invoke(vo2);
                } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                    String message = "Reflexion error : " + e.getMessage();
                    Event evt = build(original, adapter, LifeCycleConstants.EVENT_TYPE_REFLEXION_ERROR, message, null, null, null);
                    evtManager.logEvent(evt);
                    continue;
                }

                // If one of both is null.
                Object ref = o1;
                Object ref2 = o2;
                if (null == ref) {
                    ref = o2;
                    ref2 = o1;
                }

                // Not implemented type
                if ((null == ref && null == ref2) || null != getNotImplementedType(o1) || null != getNotImplementedType(o2)) {
                    continue;
                } else if (ref instanceof List) {
                    diffs = compareLists(original, diffs, parent, o1, o2, property);
                } else if (null != (clazz = getSimpleType(ref))) {
                    compareSimpleTypes(diffs, parent, clazz, o1, o2, property, ref, ref2);
                } else {
                    // Recursive
                    diffs = logDiffs(original, o1, o2, diffs, property);
                }
            }
        }

        return diffs;
    }

    /**
     * Compare two objects with simple type
     * 
     * @param diffs
     * @param parent
     * @param clazz
     * @param o1
     * @param o2
     * @param property
     * @param ref
     * @param ref2
     */
    private void compareSimpleTypes(List<DiffProperty> diffs, String parent, Class<?> clazz, Object o1, Object o2, String property, Object ref, Object ref2) {
        if (!clazz.cast(ref).equals(clazz.cast(ref2))) {
            diffs.add(createDiffProperty(property, object2string(o1), object2string(o2), parent, LifeCycleConstants.DIFF_TYPE_VALUE));
        }
    }

    /**
     * Compare two lists.
     * 
     * @param original
     * @param diffs
     * @param parent
     * @param o1
     * @param o2
     * @param property
     * @return List<DiffProperty>
     */
    private List<DiffProperty> compareLists(T original, List<DiffProperty> diffs, String parent, Object o1, Object o2, String property) {
        List<?> l1 = (List<?>) o1;
        List<?> l2 = (List<?>) o2;

        if (isNotEmpty(l1) && isNotEmpty(l2) && l1.size() == l2.size()) {
            // Recursive
            for (Integer i = 0; i < l1.size(); i++) {
                diffs = logDiffs(original, l1.get(i), l2.get(i), diffs, property);
            }
        } else if (isNotEmpty(l1) && isNotEmpty(l2)) {
            diffs.add(createDiffProperty(property, String.valueOf(l1.size()), String.valueOf(l2.size()), parent, LifeCycleConstants.DIFF_TYPE_SIZE));

            List<?> lmin = (l1.size() >= l2.size()) ? l2 : l1;
            List<?> lmax = (l1.size() >= l2.size()) ? l1 : l2;

            // Adding value
            for (Integer i = lmin.size(); i < lmax.size(); i++) {
                diffs = logDiffs(original, null, lmax.get(i), diffs, property);
            }

            // Recursive
            for (Integer i = 0; i < lmin.size(); i++) {
                diffs = logDiffs(original, l1.get(i), l2.get(i), diffs, property);
            }
        } else if (isNotEmpty(l1)) {
            diffs.add(createDiffProperty(property, String.valueOf(l1.size()), "null", parent, LifeCycleConstants.DIFF_TYPE_SIZE));

            // Adding value
            for (Integer i = 0; i < l1.size(); i++) {
                diffs = logDiffs(original, l1.get(i), null, diffs, property);
            }
        } else {
            diffs.add(createDiffProperty(property, "null", String.valueOf(l2.size()), parent, LifeCycleConstants.DIFF_TYPE_SIZE));

            // Adding value
            for (Integer i = 0; i < l2.size(); i++) {
                diffs = logDiffs(original, null, l2.get(i), diffs, property);
            }
        }
        return diffs;
    }

    /**
     * Create diff property.
     * 
     * @param name
     * @param before
     * @param after
     * @param parent
     * @param type
     * @return DiffProperty
     */
    private DiffProperty createDiffProperty(String name, String before, String after, String parent, String type) {
        DiffProperty diff = new DiffProperty();
        diff.setPropertyName(name);
        diff.setBeforeValue(before);
        diff.setAfterValue(after);
        diff.setParentPropertyName(parent);
        diff.setType(type);
        return diff;
    }

    /**
     * Get list of simple type.
     * 
     * @return List<Class<?>>
     */
    private List<Class<?>> getSimpleTypes() {
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
        rtn.add(Calendar.class);
        return rtn;
    }

    /**
     * Get list of not allowed type.
     * 
     * @return List<Class<?>>
     */
    private List<Class<?>> getNotImplementedTypes() {
        List<Class<?>> rtn = new ArrayList<Class<?>>();
        rtn.add(Map.class);
        return rtn;
    }

    /**
     * Get not implemented type.
     * 
     * @param o
     * @return Class<?>
     */
    private Class<?> getNotImplementedType(Object o) {
        return getClassFromObject(o, getNotImplementedTypes());
    }

    /**
     * Get simple type.
     * 
     * @param o
     * @return Class<?>
     */
    public Class<?> getSimpleType(Object o) {
        return getClassFromObject(o, getSimpleTypes());
    }

    /**
     * Get class from object.
     * 
     * @param o
     * @param lstClass
     * @return
     */
    private Class<?> getClassFromObject(Object o, List<Class<?>> lstClass) {
        for (Class<?> c : lstClass) {
            if (c.isInstance(o)) {
                return c;
            }
        }

        return null;
    }

    /**
     * Get common methods of two value object.
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
     * @param evtManager
     *            the evtManager to set
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
     * @param adapter
     *            the adapter to set
     */
    public void setAdapter(A adapter) {
        this.adapter = adapter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<ClassListener> getClassListeners() {
        return classListeners;
    }

    /**
     * @param classListeners
     *            the classListeners to set
     */
    public void setClassListeners(List<ClassListener> classListeners) {
        this.classListeners = classListeners;
    }
}
