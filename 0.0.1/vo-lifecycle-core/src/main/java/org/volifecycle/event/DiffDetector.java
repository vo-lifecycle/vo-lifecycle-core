package org.volifecycle.event;

import java.util.List;

import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * Diff detector interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T> value object type
 */
public interface DiffDetector<T, A extends LifeCycleAdapter<T>> {
    /**
     * Return true if there are differences between vo1 and vo2
     * 
     * @param vo1 value object 1
     * @param vo2 value object 2
     * @return true or false
     */
    boolean compare(T vo1, T vo2);

    /**
     * Return true if there are differences between vo1 and vo2
     * 
     * @param vo1 value object 1
     * @param vo2 value object 2
     * @return true or false
     */
    boolean compare(T vo1, T vo2, String parentId, String parentType);

    /**
     * @return the adapter
     */
    A getAdapter();

    /**
     * @return the evtManager
     */
    EventManager getEvtManager();

    /**
     * @return the propertyFilters
     */
    List<String> getPropertyFilters();

    /**
     * @return the classFilters
     */
    List<String> getClassFilters();
}
