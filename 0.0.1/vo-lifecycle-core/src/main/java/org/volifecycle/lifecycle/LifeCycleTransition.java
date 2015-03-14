package org.volifecycle.lifecycle;

import java.util.List;

import org.volifecycle.event.EventManager;

/**
 * Transition interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject type
 * 
 */
public interface LifeCycleTransition<T> {
	/**
	 * Verify all checker
	 * 
	 * @param valueObject
	 * @param adapter
	 * @param evtManager
	 * 
	 * @return "true" if success or "false"
	 */
	public String changeState(T valueObject, LifeCycleAdapter<T> adapter,
			EventManager evtManager);

	/**
	 * Verify all checker with a forced id list
	 * 
	 * @param valueObject
	 * @param adapter
	 * @param evtManager
	 * @param forcedCheckers
	 * @return
	 */
	String changeState(T valueObject, LifeCycleAdapter<T> adapter,
			EventManager evtManager, List<String> forcedCheckers);

	/**
	 * @return the type
	 */
	String getType();

	/**
	 * @return the description
	 */
	String getDescription();

	/**
	 * @return the target state
	 */
	String getTarget();
}
