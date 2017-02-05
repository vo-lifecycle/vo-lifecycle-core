package org.volifecycle.event;

import static org.volifecycle.utils.DateUtils.getCurrentTime;

import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * Abstract methods.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T> value object type
 */
public class EventBuilder {
  private static final Logger LOGGER = LoggerFactory.getLogger(EventBuilder.class);

  /**
   * Build an existing event.
   * 
   * @param valueObject
   * @param adapter
   * @param typeEvent
   * @param details
   * @param additionnalInformations
   * @param listEvent
   * @param failedActions
   * @return Event
   */
  public static <T> void build(Event event,
                               T valueObject,
                               LifeCycleAdapter<T> adapter,
                               String typeEvent,
                               String details,
                               Map<String, String> additionnalInformations,
                               List<Event> listEvent,
                               List<String> failedActions) {
    if (null == listEvent) {
      LOGGER.warn("listEvent is null, nothing to do");
      return;
    }

    listEvent.add(new Event.Builder(event).typeEvent(typeEvent).date(getCurrentTime())
        .details(details).idValueObject(adapter.getId(valueObject))
        .typeValueObject(adapter.getType(valueObject)).actor(adapter.getActor(valueObject))
        .additionalInformations(additionnalInformations).failedActionsIds(failedActions).build());
  }

  /**
   * Build a new Event.
   * 
   * @param valueObject
   * @param adapter
   * @param typeEvent
   * @param details
   * @param additionnalInformations
   * @param listEvent
   * @param failedActions
   * @return Event
   */
  public static <T> Event build(T valueObject,
                                LifeCycleAdapter<T> adapter,
                                String typeEvent,
                                String details,
                                Map<String, String> additionnalInformations,
                                List<Event> listEvent,
                                List<String> failedActions) {
    Event event = new Event();
    build(event,
        valueObject,
        adapter,
        typeEvent,
        details,
        additionnalInformations,
        listEvent,
        failedActions);
    return event;
  }
}
