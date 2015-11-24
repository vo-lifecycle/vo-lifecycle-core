package org.volifecycle.common;

import static org.volifecycle.utils.DateUtils.getCurrentTime;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.LogEventManagerImpl;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.LifeCycleAdapter;

/**
 * Abstract methods
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public abstract class AbstractLifeCycle<T> {
    /**
     * Log custom event
     * 
     * @param event
     * @param valueObject
     * @param adapter
     * @param typeEvent
     * @param details
     * @param List
     *            <String> failedActions
     */
    public void logCustomEvent(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, String typeEvent, String details, Map<String, String> additionnalInformations, List<String> failedActions) {
        Event event = new Event();
        setCustomEvent(event, valueObject, adapter, typeEvent, details, additionnalInformations, failedActions);

        if (null == evtManager) {
            evtManager = new LogEventManagerImpl();
        }

        evtManager.logEvent(event);
    }

    /**
     * Log custom event
     * 
     * @param event
     * @param valueObject
     * @param adapter
     * @param typeEvent
     * @param details
     */
    public void logCustomEvent(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, String typeEvent, String details, Map<String, String> additionnalInformations) {
        logCustomEvent(valueObject, adapter, evtManager, typeEvent, details, additionnalInformations, null);
    }

    /**
     * Set Custom event
     * 
     * @param event
     * @param valueObject
     * @param adapter
     * @param typeEvent
     * @param details
     * @param List
     *            <String> failedActions
     * @return Event
     */
    public void setCustomEvent(Event event, T valueObject, LifeCycleAdapter<T> adapter, String typeEvent, String details, Map<String, String> additionnalInformations, List<String> failedActions) {
        event.setTypeEvent(typeEvent);
        event.setDate(getCurrentTime());
        event.setDetails(details);

        event.setIdValueObject(adapter.getId(valueObject));
        event.setTypeValueObject(adapter.getType(valueObject));
        event.setActor(adapter.getActor(valueObject));
        event.setAdditionalInformations(additionnalInformations);
        event.setFailedActionsIds(failedActions);
    }

    /**
     * Set Custom event
     * 
     * @param event
     * @param valueObject
     * @param adapter
     * @param typeEvent
     * @param details
     * @return Event
     */
    public void setCustomEvent(Event event, T valueObject, LifeCycleAdapter<T> adapter, String typeEvent, String details, Map<String, String> additionnalInformations) {
        setCustomEvent(event, valueObject, adapter, typeEvent, details, additionnalInformations, null);
    }
}
