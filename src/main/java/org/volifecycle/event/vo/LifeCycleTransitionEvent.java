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
   * @param actionsEvents the actionsEvents to set
   */
  public void setActionsEvents(List<Event> actionsEvents) {
    this.actionsEvents = actionsEvents;
  }

  public static class Builder extends Event.Builder {
    public Builder() {
      super(new LifeCycleTransitionEvent());
    }

    public Builder(LifeCycleTransitionEvent event) {
      super(event);
    }

    public Builder actionsEvents(List<Event> actionsEvents) {
      getCastedEvent().setActionsEvents(actionsEvents);
      return this;
    }

    private LifeCycleTransitionEvent getCastedEvent() {
      return ((LifeCycleTransitionEvent) super.event);
    }

    @Override
    public LifeCycleTransitionEvent build() {
      return getCastedEvent();
    }
  }
}
