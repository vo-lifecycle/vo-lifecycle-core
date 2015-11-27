package org.volifecycle.event.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.volifecycle.utils.DateUtils.calendarToString;
import static org.volifecycle.utils.JSONUtils.map2jsonQuietly;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.event.EventManager;
import org.volifecycle.event.vo.DiffEvent;
import org.volifecycle.event.vo.Event;

/**
 * Slf4j implement of Event manager.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class LogEventManagerImpl implements EventManager {
    private static final Logger LOGGER = LoggerFactory.getLogger(LogEventManagerImpl.class);

    /**
     * {@inheritDoc}
     */
    @Override
    public void logEvent(Event e) {
        Map<String, String> logLine = new HashMap<String, String>();
        logLine.put("type", e.getTypeEvent());
        logLine.put("actor", e.getActor());
        logLine.put("date", calendarToString(e.getDate()));
        logLine.put("message", e.getDetails());

        // Case of diff event
        if (e instanceof DiffEvent) {
            DiffEvent de = (DiffEvent) e;
            logLine.put("parentId", de.getParentId());
            logLine.put("parentType", de.getParentType());

            LOGGER.info(map2jsonQuietly(logLine));

            if (isNotEmpty(de.getDiffProperties())) {
                Map<String, List<?>> logLineDiff = new HashMap<String, List<?>>();
                logLineDiff.put("diffs", de.getDiffProperties());

                LOGGER.info(map2jsonQuietly(logLineDiff));
            }
        } else {
            LOGGER.info(map2jsonQuietly(logLine));
        }
    }
}
