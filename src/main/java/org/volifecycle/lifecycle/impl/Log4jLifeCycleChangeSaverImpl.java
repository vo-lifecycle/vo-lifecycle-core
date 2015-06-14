package org.volifecycle.lifecycle.impl;

import static org.volifecycle.utils.DateUtils.calendarToString;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.volifecycle.lifecycle.LifeCycleChangeSaver;
import org.volifecycle.lifecycle.vo.LifeCycleChange;

/**
 * Log4j implementation of saver
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class Log4jLifeCycleChangeSaverImpl implements LifeCycleChangeSaver {
	private static final Logger LOGGER = LogManager
			.getLogger(Log4jLifeCycleChangeSaverImpl.class);

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void logChange(LifeCycleChange c) {
		LOGGER.info("Lifecycle id = " + c.getLifeCycleId());
		LOGGER.info("VO id = " + c.getValueObjectId(),
				"VO type = " + c.getValueObjectType());
		LOGGER.info("VO sate in = " + c.getStateIn(),
				"VO state out = " + c.getStateOut());
		LOGGER.info("Date = " + calendarToString(c.getDate()));
	}
}
