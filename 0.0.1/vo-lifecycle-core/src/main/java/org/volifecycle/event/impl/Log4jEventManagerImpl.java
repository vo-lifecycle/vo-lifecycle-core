package org.volifecycle.event.impl;

import static org.volifecycle.utils.DateUtils.calendarToString;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.Event;

/**
 * Log4j implement of Event manager
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Log4jEventManagerImpl implements EventManager {
    private static final Logger LOGGER = LogManager.getLogger(Log4jEventManagerImpl.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void logEvent(Event e) {
        LOGGER.info("type = " + e.getTypeEvent() + ", actor = " + e.getActor() + ", date = " + calendarToString(e.getDate()));
        LOGGER.info("VO id = " + e.getIdValueObject(), "VO type = " + e.getTypeValueObject());
        LOGGER.info("message = " + e.getDetails());
    }
}
