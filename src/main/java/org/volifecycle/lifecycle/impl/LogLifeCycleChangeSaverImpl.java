package org.volifecycle.lifecycle.impl;

import static org.volifecycle.utils.DateUtils.calendarToString;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.lifecycle.LifeCycleChangeSaver;
import org.volifecycle.lifecycle.vo.LifeCycleChange;

/**
 * Slf4j implementation of saver.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class LogLifeCycleChangeSaverImpl implements LifeCycleChangeSaver {
    private static final Logger LOGGER = LoggerFactory.getLogger(LogLifeCycleChangeSaverImpl.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void logChange(LifeCycleChange c) {
        LOGGER.info("Lifecycle id = " + c.getLifeCycleId());
        LOGGER.info("VO id = " + c.getValueObjectId(), "VO type = " + c.getValueObjectType());
        LOGGER.info("VO sate in = " + c.getStateIn(), "VO state out = " + c.getStateOut());
        LOGGER.info("Date = " + calendarToString(c.getDate()));
    }
}
