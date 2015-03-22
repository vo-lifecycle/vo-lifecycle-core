package org.volifecycle.lifecycle.impl;

import static org.volifecycle.utils.DateUtils.getCurrentTime;

import java.util.List;
import java.util.Map;

import org.volifecycle.event.EventManager;
import org.volifecycle.event.impl.Log4jEventManagerImpl;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleChangeSaver;
import org.volifecycle.lifecycle.LifeCycleConstants;
import org.volifecycle.lifecycle.LifeCycleManager;
import org.volifecycle.lifecycle.LifeCycleState;
import org.volifecycle.lifecycle.LifeCycleTransition;
import org.volifecycle.lifecycle.vo.LifeCycleChange;

/**
 * Implementation of manager
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject type
 */
public class LifeCycleManagerImpl<T, A extends LifeCycleAdapter<T>> implements
		LifeCycleManager<T, LifeCycleAdapter<T>> {
	/**
	 * States by id
	 */
	protected Map<String, LifeCycleState<T>> statesById;

	/**
	 * The adapter
	 */
	protected A adapter;

	/**
	 * The event manager
	 */
	protected EventManager evtManager;

	/**
	 * The life cycle change saver
	 */
	protected LifeCycleChangeSaver saver;

	/**
	 * Id
	 */
	protected String id;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String runTransition(String idTransition, T valueObject,
			List<String> forcedCheckers) {
		EventManager eManager = getEvtManager();

		// Default EventManager
		if (null == eManager) {
			eManager = new Log4jEventManagerImpl();
		}

		if (null == valueObject) {
			throw new IllegalStateException("Value object must not be null");
		}

		String keyState = adapter.getState(valueObject);

		// Searching current state in transco
		if (null == keyState || !statesById.containsKey(keyState)) {
			throw new IllegalStateException("Unknown state "
					+ ((null == keyState) ? "<null>" : keyState));
		}
		LifeCycleState<T> currentState = statesById.get(keyState);

		// Searching the current state's transitions
		Map<String, LifeCycleTransition<T>> transitionsById = currentState
				.getTransitionsById();

		// Searching the requested id of transition in the current state's
		// allowed transitions
		if (null == transitionsById
				|| !transitionsById.containsKey(idTransition)) {
			throw new IllegalStateException("Unknown transition ("
					+ idTransition + ") for state " + keyState);
		}

		LifeCycleTransition<T> transition = transitionsById.get(idTransition);
		String rtn = transition.changeState(valueObject, adapter, eManager,
				forcedCheckers);
		String targetState = transition.getTarget();

		// Change state (in database or other persistence support)
		if (null != targetState
				&& !LifeCycleConstants.FALSE.equalsIgnoreCase(rtn)) {
			adapter.setState(valueObject, targetState);
			logChangeCustom(valueObject, idTransition, keyState, adapter,
					targetState);
		}

		return rtn;
	}

	/**
	 * Log change
	 * 
	 * @param valueObject
	 * @param transitionId
	 * @param stateIn
	 * @param adapter
	 * @param stateOut
	 */
	public void logChangeCustom(T valueObject, String transitionId,
			String stateIn, LifeCycleAdapter<T> adapter, String stateOut) {
		LifeCycleChangeSaver s = getSaver();
		if (null == s) {
			s = new Log4jLifeCycleChangeSaverImpl();
		}

		LifeCycleChange c = new LifeCycleChange();
		c.setDate(getCurrentTime());
		c.setStateIn(stateIn);
		c.setStateIn(stateOut);
		c.setValueObjectId(adapter.getId(valueObject));
		c.setValueObjectType(adapter.getType(valueObject));
		c.setTransitionId(transitionId);
		s.logChange(c);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String runTransition(String idTransition, T valueObject) {
		return runTransition(idTransition, valueObject, null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, LifeCycleState<T>> getStatesById() {
		return statesById;
	}

	/**
	 * @param statesById
	 *            the statesById to set
	 */
	public void setStatesById(Map<String, LifeCycleState<T>> statesById) {
		this.statesById = statesById;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public A getAdapter() {
		return adapter;
	}

	/**
	 * @param adapter
	 *            the adapter to set
	 */
	public void setAdapter(A adapter) {
		this.adapter = adapter;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getDescription() {
		return description;
	}

	/**
	 * @param description
	 *            the description to set
	 */
	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public EventManager getEvtManager() {
		return evtManager;
	}

	/**
	 * @param evtManager
	 *            the evtManager to set
	 */
	public void setEvtManager(EventManager evtManager) {
		this.evtManager = evtManager;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getId() {
		return id;
	}

	/**
	 * @param id
	 *            the id to set
	 */
	public void setId(String id) {
		this.id = id;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public LifeCycleChangeSaver getSaver() {
		return saver;
	}

	/**
	 * @param saver
	 *            the saver to set
	 */
	public void setSaver(LifeCycleChangeSaver saver) {
		this.saver = saver;
	}
}
