package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;
import java.util.Map;

import org.volifecycle.common.LifeCycleConstants;
import org.volifecycle.lifecycle.LifeCycleChecker;
import org.volifecycle.lifecycle.LifeCyclePredicate;

/**
 * Implementation of checker
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 * @param <T>
 *            valueObject
 */
public abstract class LifeCycleCheckerImpl<T> implements LifeCycleChecker<T> {
	/**
	 * Id which is used for forced the result of this checker
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
	 * List predicates wich are executed by "and"
	 */
	protected List<LifeCyclePredicate<T>> predicates;

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
	 * @return the targetState
	 */
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
	 * @return the predicates
	 */
	public List<LifeCyclePredicate<T>> getPredicates() {
		return predicates;
	}

	/**
	 * @param predicates
	 *            the predicates to set
	 */
	public void setPredicates(List<LifeCyclePredicate<T>> predicates) {
		this.predicates = predicates;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String getResult(T valueObject, List<String> failedPredicate) {
		return getResult(valueObject, null);
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
	public String getResult(T valueObject, List<String> failedPredicate, Map<String, Object> actionStorage) {
		String rtn = getTargetState();
		String result;

		if (isNotEmpty(predicates)) {
			for (LifeCyclePredicate<T> predicate : predicates) {
				result = predicate.getResult(valueObject, actionStorage);
				if (LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
					rtn = LifeCycleConstants.FALSE;

					if (null != failedPredicate && !failedPredicate.contains(predicate.getId())) {
						failedPredicate.add(predicate.getId());
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
