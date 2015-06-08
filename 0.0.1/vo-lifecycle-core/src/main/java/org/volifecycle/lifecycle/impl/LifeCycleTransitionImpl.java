package org.volifecycle.lifecycle.impl;

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
import org.volifecycle.lifecycle.LifeCycleChecker;
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
	 * List of composite checkers.
	 */
	protected List<LifeCycleChecker<T>> checkers;

	/**
	 * Action storage.
	 */
	protected LifeCycleActionStorage<T> actionStorage;

	/**
	 * auto | manual
	 */
	protected String type;

	/**
	 * Description
	 */
	protected String description;

	/**
	 * @return the checkers
	 */
	public List<LifeCycleChecker<T>> getCheckers() {
		return checkers;
	}

	/**
	 * @param checkers
	 *            the checkers to set
	 */
	public void setCheckers(List<LifeCycleChecker<T>> checkers) {
		this.checkers = checkers;
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
	public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager) {
		return changeState(valueObject, adapter, evtManager, null);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String changeState(T valueObject, LifeCycleAdapter<T> adapter, EventManager evtManager, List<String> forcedCheckers) {
		String rtn = null;
		Map<String, String> additionnalInformations = null;

		Map<String, Object> actionStorageResult = null;
		if (null != actionStorage) {
			actionStorageResult = actionStorage.getActionStorageResult(valueObject);
		}

		if (isNotEmpty(checkers)) {
			for (LifeCycleChecker<T> checker : checkers) {
				boolean filter = false;

				// Searching if is a checker to ignore
				if (isNotEmpty(forcedCheckers)) {
					for (String idChecker : forcedCheckers) {
						if (null != checker.getId() && idChecker.equalsIgnoreCase(checker.getId())) {
							filter = true;
							break;
						}
					}
				}

				additionnalInformations = checker.getAdditionnalInformations();
				List<String> failedPredicates = new ArrayList<String>();
				String result = checker.getResult(valueObject, failedPredicates, actionStorageResult);

				if (null == result || LifeCycleConstants.FALSE.equalsIgnoreCase(result)) {
					if (!filter) {
						rtn = LifeCycleConstants.FALSE;
						String message = "Failed checker : " + checker.getId() + ", predicate : " + implode(",", failedPredicates);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FAILED_CHECKER, message, additionnalInformations);
					} else {
						String message = "Forced checker : " + checker.getId() + ", predicate : " + implode(",", failedPredicates);
						logCustomEvent(valueObject, adapter, evtManager, LifeCycleConstants.EVENT_TYPE_FORCED_CHECKER, message, additionnalInformations);
					}
				} else if (!LifeCycleConstants.FALSE.equalsIgnoreCase(rtn)) {
					rtn = result;
				}
			}
		}

		return (null == rtn) ? LifeCycleConstants.FALSE : rtn;
	}
}
