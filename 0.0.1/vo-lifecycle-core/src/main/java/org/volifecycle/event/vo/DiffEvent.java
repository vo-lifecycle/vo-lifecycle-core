package org.volifecycle.event.vo;

import java.util.List;

/**
 * Diff event class
 * 
 * @author Idriss Neumann <neumann.idriss@gmail.com>
 * 
 */
public class DiffEvent extends Event {
    private static final long serialVersionUID = 1L;

    /**
     * List of diff properties
     */
    List<DiffProperty> diffProperties;

    /**
     * Parent vo id
     */
    String parentId;

    /**
     * Parent vo type
     */
    String parentType;

    /**
     * @return the diffProperties
     */
    public List<DiffProperty> getDiffProperties() {
        return diffProperties;
    }

    /**
     * @param diffProperties the diffProperties to set
     */
    public void setDiffProperties(List<DiffProperty> diffProperties) {
        this.diffProperties = diffProperties;
    }

    /**
     * @return the parentId
     */
    public String getParentId() {
        return parentId;
    }

    /**
     * @param parentId the parentId to set
     */
    public void setParentId(String parentId) {
        this.parentId = parentId;
    }

    /**
     * @return the parentType
     */
    public String getParentType() {
        return parentType;
    }

    /**
     * @param parentType the parentType to set
     */
    public void setParentType(String parentType) {
        this.parentType = parentType;
    }
}
