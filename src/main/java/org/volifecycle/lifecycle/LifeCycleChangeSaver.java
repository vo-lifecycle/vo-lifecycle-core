package org.volifecycle.lifecycle;

import org.volifecycle.lifecycle.vo.LifeCycleChange;

/**
 * LifeCycle change saver interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public interface LifeCycleChangeSaver {
    /**
     * Saving change.
     * 
     * @param c
     *            lifeCycleChange instance
     */
    void logChange(LifeCycleChange c);
}
