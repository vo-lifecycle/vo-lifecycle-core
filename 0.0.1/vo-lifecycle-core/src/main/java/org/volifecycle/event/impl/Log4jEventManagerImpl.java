package org.volifecycle.event.impl;

import static org.volifecycle.utils.DateUtils.calendarToString;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.Event;
import org.volifecycle.lifecycle.impl.LifeCycleTransitionImpl;

/**
 * Log4j implement of Event manager
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Log4jEventManagerImpl implements EventManager {
	private static final Logger LOGGER = LogManager
			.getLogger(LifeCycleTransitionImpl.class);

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void logEvent(Event e) {
		LOGGER.info("type = " + e.getTypeEvent() + ", actor = " + e.getActor() + ", date = " + calendarToString(e.getDate()));
		LOGGER.info("message = " + e.getMessage());
	}
}
