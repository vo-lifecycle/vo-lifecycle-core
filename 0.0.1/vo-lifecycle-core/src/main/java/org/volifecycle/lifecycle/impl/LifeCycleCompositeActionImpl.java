package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;
import java.util.Map;

import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.lifecycle.LifeCycleAction;
import org.volifecycle.lifecycle.LifeCycleCompositeAction;

/**
 * Implementation of composite action.
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject
 */
public abstract class LifeCycleCompositeActionImpl<T> implements LifeCycleCompositeAction<T> {
	/**
	 * Id which is used for forced the result of this action
	 */
	protected String id;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * target state
	 */
	protected String targetState;

	/**
	 * List actions wich are executed by "and" predicate operator.
	 */
	protected List<LifeCycleAction<T>> simpleActions;

	/**
	 * Setting with true if you want stop when a predicate failed.
	 */
	protected Boolean stopIfFailed;

	/**
	 * Additionnal informations to save.
	 */
	protected Map<String, String> additionnalInformations;

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
	public String getTargetState() {
		return targetState;
	}

	/**
	 * @param targetState
	 *            the targetState to set
	 */
	public void setTargetState(String targetState) {
		this.targetState = targetState;
	}

	/**
	 * @return the simpleActions
	 */
	public List<LifeCycleAction<T>> getSimpleActions() {
		return simpleActions;
	}

	/**
	 * @param simpleActions
	 *            the simpleActions to set
	 */
	public void setSimpleActions(List<LifeCycleAction<T>> simpleActions) {
		this.simpleActions = simpleActions;
	}

	/**
	 * @return the stopIfFailed
	 */
	public Boolean getStopIfFailed() {
		return stopIfFailed;
	}

	/**
	 * @param stopIfFailed
	 *            the stopIfFailed to set
	 */
	public void setStopIfFailed(Boolean stopIfFailed) {
		this.stopIfFailed = stopIfFailed;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Map<String, String> getAdditionnalInformations() {
		return additionnalInformations;
	}

	/**
	 * @param additionnalInformations
	 *            the additionnalInformations to set
	 */
	public void setAdditionnalInformations(Map<String, String> additionnalInformations) {
		this.additionnalInformations = additionnalInformations;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject) {
		return getResult(valueObject, (List<String>) null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject, List<String> failedPredicate) {
		return getResult(valueObject, failedPredicate, null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject, Map<String, Object> actionStorage) {
		return getResult(valueObject, null, actionStorage);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject, List<String> failedSubActions, Map<String, Object> actionStorage) {
		String rtn = getTargetState();
		String result;

		if (isNotEmpty(simpleActions)) {
			for (LifeCycleAction<T> action : simpleActions) {
				result = action.getResult(valueObject, actionStorage);
				if (LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
					rtn = LifeCycleConstants.FALSE;

					if (null != failedSubActions && !failedSubActions.contains(action.getId())) {
						failedSubActions.add(action.getId());
					}

					if (null != stopIfFailed && stopIfFailed) {
						break;
					}
				}
			}
		}

		return rtn;
	}
}
