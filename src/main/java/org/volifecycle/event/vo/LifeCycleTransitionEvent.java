package org.volifecycle.event.vo;

import java.util.List;

/**
 * Transition events.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class LifeCycleTransitionEvent extends Event {
    private static final long serialVersionUID = 1L;

    List<Event> actionsEvents;

    /**
     * @return the subActionsEvents
     */
    public List<Event> getActionsEvents() {
        return actionsEvents;
    }

    /**
     * @param subActionsEvents
     *            the subActionsEvents to set
     */
    public void setActionsEvents(List<Event> subActionsEvents) {
        this.actionsEvents = subActionsEvents;
    }
}
