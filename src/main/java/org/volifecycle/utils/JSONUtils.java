package org.volifecycle.utils;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.apache.commons.collections.MapUtils.isNotEmpty;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Utils class in order to Serialize and Deserialize JSON content.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class JSONUtils {
    private static final Logger LOGGER = LoggerFactory.getLogger(JSONUtils.class);
    private static final ObjectMapper MAPPER = new ObjectMapper();

    /**
     * Static class
     */
    private JSONUtils() {

    }

    /**
     * Convert map to JSON string.
     * 
     * @param map
     * @return String
     */
    public static String map2jsonQuietly(Map<?, ?> map) {
        if (!isNotEmpty(map)) {
            return StringUtils.EMPTY;
        }

        return writeQuietly(map);
    }

    /**
     * Convert list to JSON string.
     * 
     * @param list
     * @return String
     */
    public static String list2jsonQuietly(List<? extends List<String>> list) {
        if (!isNotEmpty(list)) {
            return StringUtils.EMPTY;
        }

        return writeQuietly(list);
    }

    /**
     * Convert Object to JSON String.
     * 
     * @param list
     * @return
     */
    private static String writeQuietly(Object list) {
        String rtn = StringUtils.EMPTY;

        try {
            rtn = MAPPER.writeValueAsString(list);
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
                rtn = MAPPER.readValue(json, new TypeReference<HashMap<String, String>>() {
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
        String json = null;
        try {
            json = MAPPER.writeValueAsString(clazz.cast(obj));
        } catch (Exception e) {
            LOGGER.error("Parsing error", e);
        }

        return json;
    }

}
