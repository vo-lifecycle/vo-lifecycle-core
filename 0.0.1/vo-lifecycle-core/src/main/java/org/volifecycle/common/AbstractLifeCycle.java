package org.volifecycle.common;

import static org.volifecycle.utils.DateUtils.getCurrentTime;

import org.volifecycle.event.EventManager;
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
	 */
	public void logCustomEvent(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, String typeEvent, String details) {
		Event event = new Event();
		setCustomEvent(event, valueObject, adapter, typeEvent, details);
		evtManager.logEvent(event);
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
	public void setCustomEvent(Event event, T valueObject, LifeCycleAdapter<T> adapter, String typeEvent, String details) {
		event.setTypeEvent(typeEvent);
		event.setDate(getCurrentTime());
		event.setDetails(details);

		event.setIdValueObject(adapter.getId(valueObject));
		event.setTypeValueObject(adapter.getType(valueObject));
		event.setActor(adapter.getActor(valueObject));
	}
}
