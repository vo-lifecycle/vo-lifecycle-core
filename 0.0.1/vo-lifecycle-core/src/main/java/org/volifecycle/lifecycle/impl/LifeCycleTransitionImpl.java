package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isEmpty;
import static org.apache.commons.collections.CollectionUtils.isNotEmpty;
import static org.volifecycle.utils.CommonUtils.implode;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.volifecycle.common.AbstractLifeCycle;
import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.event.EventManager;
import org.volifecycle.lifecycle.LifeCycleActionStorage;
import org.volifecycle.lifecycle.LifeCycleAdapter;
import org.volifecycle.lifecycle.LifeCycleCompositeAction;
import org.volifecycle.lifecycle.LifeCycleTransition;

/**
 * Transition implementation
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            value object type
 */
public class LifeCycleTransitionImpl<T> extends AbstractLifeCycle<T> implements LifeCycleTransition<T> {
	/**
	 * List of composite actions.
	 */
	protected List<LifeCycleCompositeAction<T>> actions;

	/**
	 * Action storage.
	 */
	protected LifeCycleActionStorage<T> actionStorage;

	/**
	 * The id.
	 */
	protected String id;

	/**
	 * auto | manual
	 */
	protected String type;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * List of targeted states.
	 */
	List<String> targetStates;

	/**
	 * @return the actions
	 */
	public List<LifeCycleCompositeAction<T>> getActions() {
		return actions;
	}

	/**
	 * @param actions
	 *            the actions to set
	 */
	public void setActions(List<LifeCycleCompositeAction<T>> actions) {
		this.actions = actions;
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
	public String getType() {
		return type;
	}

	/**
	 * @param type
	 *            the type to set
	 */
	public void setType(String type) {
		this.type = type;
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
	 * @return the actionStorage
	 */
	public LifeCycleActionStorage<T> getActionStorage() {
		return actionStorage;
	}

	/**
	 * @param actionStorage
	 *            the actionStorage to set
	 */
	public void setActionStorage(LifeCycleActionStorage<T> actionStorage) {
		this.actionStorage = actionStorage;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public List<String> getTargetStates() {
		return targetStates;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setTargetStates(List<String> targetStates) {
		this.targetStates = targetStates;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager) {
		return changeState(valueObject, adapter, evtManager, null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedActions) {
		String rtn = null;
		Map<String, String> additionnalInformations = null;

		Map<String, Object> actionStorageResult = null;
		if (null != actionStorage) {
			actionStorageResult = actionStorage.getActionStorageResult(valueObject);
		}

		if (isNotEmpty(actions)) {
			for (LifeCycleCompositeAction<T> action : actions) {
				boolean filter = false;

				// Searching if is an action to ignore
				if (isNotEmpty(forcedActions)) {
					for (String idForcedAction : forcedActions) {
						if (null != action.getId() && idForcedAction.equalsIgnoreCase(action.getId())) {
							filter = true;
							break;
						}
					}
				}

				additionnalInformations = action.getAdditionnalInformations();
				List<String> failedSimpleActions = new ArrayList<String>();
				String result = action.getResult(valueObject, failedSimpleActions, actionStorageResult);

				if (null == result || LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
					LifeCycleCompositeAction<T> compositeAction = null;
					if (!filter) {
						rtn = LifeCycleConstants.FALSE;
						String message = "Failed action : " + action.getId() + ", sub actions : " + implode(",", failedSimpleActions);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FAILED_ACTION, message, additionnalInformations);
					} else if (action instanceof LifeCycleCompositeAction<?>) {
						compositeAction = (LifeCycleCompositeAction<T>) action;
						rtn = compositeAction.getTargetState();
						String message = "Forced action : " + action.getId() + ", sub actions : " + implode(",", failedSimpleActions);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FORCED_ACTION, message, additionnalInformations);
					} else if (isNotEmpty(targetStates) && targetStates.size() == 1) {
						rtn = targetStates.get(0);
						String message = "Forced action : " + action.getId() + " (only one target state), sub actions : " + implode(",", failedSimpleActions);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FORCED_ACTION, message, additionnalInformations);
					} else {
						rtn = LifeCycleConstants.FALSE;
						String message = "Failed action : " + action.getId() + " (no target state), sub actions : " + implode(",", failedSimpleActions);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FAILED_ACTION, message, additionnalInformations);
					}
				} else if (!LifeCycleConstants.FALSE.equalsIgnoreCase(rtn)) {
					rtn = result;
				}
			}
		}

		if (LifeCycleConstants.TRUE.equalsIgnoreCase(rtn) && isNotEmpty(targetStates) && targetStates.size() == 1) {
			rtn = targetStates.get(0);
		} else if (isEmpty(targetStates) || null == rtn || LifeCycleConstants.FALSE.equals(rtn) || !targetStates.contains(rtn)) {
			rtn = LifeCycleConstants.FALSE;
			String message = "Failed transition : id=" + this.getId() + ", targetStates = " + implode(",", targetStates);
			logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FAILED_TRANSITION, message, additionnalInformations);
		}

		return rtn;
	}
}
