package org.volifecycle.common;

/**
 * Constants
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public interface LifeCycleConstants {
	static final String TRUE = "true";
	static final String FALSE = "false";
	static final String AUTO = "auto";
	static final String MANUAL = "manual";
	static final String SYS_ACTOR = "system";
	static final String EMPTY_STRING = "";

	/**
	 * Types of event
	 */
	static final String EVENT_TYPE_FAILURE = "failure";
	static final String EVENT_TYPE_AUTH = "auth";
	static final String EVENT_TYPE_OTHER = "other";
	static final String EVENT_TYPE_FORCED_CHECKER = "forced_checker";
	static final String EVENT_TYPE_FAILED_CHECKER = "failed_checker";
	static final String EVENT_TYPE_REFLEXION_ERROR = "reflexion_error";
	static final String EVENT_TYPE_DIFF_VO = "diff_vo";

	/**
	 * Types of diff
	 */
	static final String DIFF_TYPE_VALUE = "value";
	static final String DIFF_TYPE_SIZE = "size";
}
