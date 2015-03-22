package org.volifecycle.lifecycle;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.EventManager;

/**
 * Manager interface
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            ValueObject type
 * @param <A>
 *            Adapter type
 */
public interface LifeCycleManager<T, A extends LifeCycleAdapter<T>> {
	/**
	 * Run a transition on valueObject from a transition configuration id
	 * 
	 * @param transitionId
	 * @param valueObject
	 * @return String "true" or "false"
	 */
	String runTransition(String transitionId, T valueObject);

	/**
	 * Run a transition on valueObject from a transition configuration id and
	 * forced checkers list
	 * 
	 * @param idTransition
	 * @param valueObject
	 * @param forcedCheckers
	 * @return String "true" or "false"
	 */
	String runTransition(String idTransition, T valueObject,
			List<String> forcedCheckers);

	/**
	 * @return the description
	 */
	String getDescription();

	/**
	 * @return the id
	 */
	String getId();

	/**
	 * @return the saver
	 */
	LifeCycleChangeSaver getSaver();

	/**
	 * @return the evtManager
	 */
	EventManager getEvtManager();

	/**
	 * @return the adapter
	 */
	A getAdapter();

	/**
	 * @return the statesById
	 */
	Map<String, LifeCycleState<T>> getStatesById();
}
