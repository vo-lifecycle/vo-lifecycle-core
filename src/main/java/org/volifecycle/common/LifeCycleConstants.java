package org.volifecycle.common;

/**
 * Constants interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public interface LifeCycleConstants {
    String AUTO = "auto";
    String MANUAL = "manual";
    String SYS_ACTOR = "system";
    String EMPTY_STRING = "";

    /**
     * Types of event
     */
    String EVENT_TYPE_FAILURE = "failure";
    String EVENT_TYPE_AUTH = "auth";
    String EVENT_TYPE_OTHER = "other";
    String EVENT_TYPE_FORCED_ACTION = "forced_action";
    String EVENT_TYPE_FAILED_ACTION = "failed_action";
    String EVENT_TYPE_FAILED_TRANSITION = "failed_transition";
    String EVENT_TYPE_SUCCESS_TRANSITION = "success_transition";
    String EVENT_TYPE_REFLEXION_ERROR = "reflexion_error";
    String EVENT_TYPE_DIFF_VO = "diff_vo";
    String EVENT_TYPE_JSR303_FAILURE = "jsr303_failure";

    /**
     * Types of diff
     */
    String DIFF_TYPE_VALUE = "value";
    String DIFF_TYPE_SIZE = "size";

    /**
     * Key in additionnals informations
     */
    String KEY_ADDI_SOURCE_ACTION_ID = "SOURCE_ACTION_ID";
}
