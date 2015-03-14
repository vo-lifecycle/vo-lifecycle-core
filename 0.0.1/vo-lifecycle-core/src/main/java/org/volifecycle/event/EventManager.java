package org.volifecycle.event;

import org.volifecycle.event.vo.Event;

/**
 * Event manager interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public interface EventManager {
	/**
	 * Log event
	 * 
	 * @param e
	 */
	void logEvent(Event e);
}
