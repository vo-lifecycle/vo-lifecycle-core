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

  public static class Builder extends Event.Builder {
    public Builder() {
      super(new DiffEvent());
    }

    public Builder(DiffEvent event) {
      super(event);
    }

    public Builder diffProperties(List<DiffProperty> diffProperties) {
      getCastedEvent().setDiffProperties(diffProperties);
      return this;
    }

    public Builder parentId(String parentId) {
      getCastedEvent().setParentId(parentId);
      return this;
    }

    public Builder parentType(String parentType) {
      getCastedEvent().setParentType(parentType);
      return this;
    }

    private DiffEvent getCastedEvent() {
      return ((DiffEvent) super.event);
    }

    @Override
    public DiffEvent build() {
      return getCastedEvent();
    }
  }
}
