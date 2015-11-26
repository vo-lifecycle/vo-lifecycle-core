package org.volifecycle.event.vo;

import java.util.List;

/**
 * Transition event.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class LifeCycleTransitionEvent extends Event {
    private static final long serialVersionUID = 1L;

    List<Event> subActionsEvents;

    /**
     * @return the subActionsEvents
     */
    public List<Event> getSubActionsEvents() {
        return subActionsEvents;
    }

    /**
     * @param subActionsEvents
     *            the subActionsEvents to set
     */
    public void setSubActionsEvents(List<Event> subActionsEvents) {
        this.subActionsEvents = subActionsEvents;
    }
}
