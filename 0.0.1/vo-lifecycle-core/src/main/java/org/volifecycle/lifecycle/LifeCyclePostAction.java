package org.volifecycle.lifecycle;

/**
 * Post action which is executed when the wired checker success.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public interface LifeCyclePostAction {
    /**
     * Execute the post action
     */
    void execute();

    /**
     * Return the id.
     * 
     * @return the id.
     */
    String getId();
}
