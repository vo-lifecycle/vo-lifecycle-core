package org.volifecycle.event;

import static org.volifecycle.utils.DateUtils.getCurrentTime;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * Abstract methods.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public class EventBuilder {
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
    public static <T> void build(Event event, T valueObject, LifeCycleAdapter<T> adapter, String typeEvent, String details, Map<String, String> additionnalInformations, List<Event> listEvent, List<String> failedActions) {
        event.setTypeEvent(typeEvent);
        event.setDate(getCurrentTime());
        event.setDetails(details);

        event.setIdValueObject(adapter.getId(valueObject));
        event.setTypeValueObject(adapter.getType(valueObject));
        event.setActor(adapter.getActor(valueObject));
        event.setAdditionalInformations(additionnalInformations);
        event.setFailedActionsIds(failedActions);

        if (null != listEvent) {
            listEvent.add(event);
        }
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
    public static <T> Event build(T valueObject, LifeCycleAdapter<T> adapter, String typeEvent, String details, Map<String, String> additionnalInformations, List<Event> listEvent, List<String> failedActions) {
        Event event = new Event();
        build(event, valueObject, adapter, typeEvent, details, additionnalInformations, listEvent, failedActions);
        return event;
    }
}
