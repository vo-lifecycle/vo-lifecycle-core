package org.volifecycle.utils;

import static org.apache.commons.collections.CollectionUtils.isEmpty;

import java.util.List;

import org.volifecycle.common.LifeCycleConstants;

/**
 * Utils common methods.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 */
public class CommonUtils {
    /**
     * Implode list with separator.
     * 
     * @param separator
     * @param data
     * @return String
     */
    public static String implode(String separator, List<String> data) {
        if (isEmpty(data)) {
            return LifeCycleConstants.EMPTY_STRING;
        }

        StringBuilder sb = new StringBuilder();
        // data.length - 1 => to not add separator at the end
        for (int i = 0; i < data.size() - 1; i++) {
            // filter empty string (which are ""; " "; "  "; and)
            if (!data.get(i).matches(" *")) {
                // so on
                sb.append(data.get(i));
                sb.append(separator);
            }
        }

        sb.append(data.get(data.size() - 1).trim());
        return sb.toString();
    }
}
