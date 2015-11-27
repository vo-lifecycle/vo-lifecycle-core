package org.volifecycle.utils;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.apache.commons.collections.MapUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.volifecycle.common.LifeCycleConstants;

/**
 * Utils class in order to Serialize and Deserialize JSON content.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class JSONUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(JSONUtils.class);

    /**
     * Convert map to JSON string.
     * 
     * @param map
     */
    public static String map2jsonQuietly(Map<?, ?> map) {
        if (!isNotEmpty(map)) {
            return LifeCycleConstants.EMPTY_STRING;
        }

        String rtn = LifeCycleConstants.EMPTY_STRING;

        try {
            ObjectMapper mapper = new ObjectMapper();
            rtn = mapper.writeValueAsString(map);
        } catch (IOException e) {
            LOGGER.error("Parsing error", e);
        }

        return rtn;
    }

    /**
     * Convert JSON String to list.
     * 
     * @param list
     * @return String
     */
    public static String list2jsonQuietly(List<? extends List<String>> list) {
        if (!isNotEmpty(list)) {
            return LifeCycleConstants.EMPTY_STRING;
        }

        String rtn = LifeCycleConstants.EMPTY_STRING;

        try {
            ObjectMapper mapper = new ObjectMapper();
            rtn = mapper.writeValueAsString(list);
        } catch (IOException e) {
            LOGGER.error("Parsing error", e);
        }

        return rtn;
    }

    /**
     * Convert JSON to map.
     * 
     * @param json
     * @return Map<String, String>
     */
    public static Map<String, String> json2mapQuietly(String json) {
        Map<String, String> rtn = null;

        if (isNotEmpty(json)) {
            try {
                ObjectMapper mapper = new ObjectMapper();
                rtn = mapper.readValue(json, new TypeReference<HashMap<String, String>>() {
                });
            } catch (Exception e) {
                LOGGER.error("Parsing error", e);
            }
        }

        return rtn;
    }

    /**
     * Convert object to JSON.
     * 
     * @param obj
     * @param clazz
     * @return String
     */
    public static String object2jsonQuietly(Object obj, Class<?> clazz) {
        ObjectMapper mapper = new ObjectMapper();
        String json = null;
        try {
            json = mapper.writeValueAsString(clazz.cast(obj));
        } catch (Exception e) {
            LOGGER.error("Parsing error", e);
        }

        return json;
    }

    /**
     * Static class
     */
    private JSONUtils() {

    }
}
