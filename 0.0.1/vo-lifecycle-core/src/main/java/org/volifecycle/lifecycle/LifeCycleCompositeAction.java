package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

/**
 * Composite action interface.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public interface LifeCycleCompositeAction<T> extends LifeCycleAction<T> {
	/**
	 * Return the result.
	 * 
	 * @param valueObject
	 * @param failedSubActions
	 * @return "true" if success or "false"
	 */
	String getResult(T valueObject, List<String> failedSubActions);

	/**
	 * Return the result.
	 * 
	 * @param valueObject
	 * @param failedPredicate
	 * @param actionStorage
	 * @return "true" if success or "false"
	 */
	String getResult(T valueObject, List<String> failedPredicate, Map<String, Object> actionStorage);

	/**
	 * @return the targetState
	 */
	String getTargetState();
}
