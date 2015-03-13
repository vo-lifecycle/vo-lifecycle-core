package org.volifecycle.lifecycle.impl;

import static org.apache.commons.collections.CollectionUtils.isNotEmpty;

import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.volifecycle.constants.Constants;
import org.volifecycle.lifecycle.LifeCycleChecker;
import org.volifecycle.lifecycle.LifeCycleTransition;

/**
 * Implémentation du service de transition
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 *
 * @param <T>
 */
public class LifeCycleTransitionImpl<T> implements LifeCycleTransition<T> {
    private static final Logger LOGGER = LogManager.getLogger(LifeCycleTransitionImpl.class);

    protected List<LifeCycleChecker<T>> checkers;
    protected String target;

    /**
     * Automatique | manuel
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
     * @param checkers the checkers to set
     */
    public void setCheckers(List<LifeCycleChecker<T>> checkers) {
        this.checkers = checkers;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getTarget() {
        return target;
    }

    /**
     * @param target the target to set
     */
    public void setTarget(String target) {
        this.target = target;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getType() {
        return type;
    }

    /**
     * @param type the type to set
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
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject) {
        return changeState(valueObject, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String changeState(T valueObject, List<String> forcedCheckers) {
        String rtn = Constants.TRUE;

        for (LifeCycleChecker<T> checker : checkers) {
            boolean filter = false;

            // Recherche des checker à ignorer
            if (isNotEmpty(forcedCheckers)) {
                for (String idChecker : forcedCheckers) {
                    if (null != checker.getId() && idChecker.equalsIgnoreCase(checker.getId())) {
                        filter = true;
                        break;
                    }
                }
            }

            String result = checker.getResult(valueObject);
            if (null == result || Constants.FALSE.equalsIgnoreCase(result)) {
                if (!filter) {
                    rtn = Constants.FALSE;
                    break;
                } else {
                    // Log à remplacer par une insertion en bdd
                    LOGGER.debug("Forced checker : " + checker.getId());
                }
            }
        }

        return rtn;
    }
}
