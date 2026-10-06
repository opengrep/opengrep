# A block in a method whose signature is extracted more than once, in a
# fixpoint over the methods calling each other, finds the flow through
# [project_id] once that method's signature is known.
class IssuableFinder
  class Params < SimpleDelegator
    def project?
      project_id.present?
    end

    def related_groups
      project.group.self_and_ancestors if project?
    end

    def project
      strong_memoize(:project) do
        next nil unless project?

        # ruleid: lambda-sig-ruby-reanalysis
        project = project_id.is_a?(Project) ? project_id : Project.find(project_id)
        project = nil unless Ability.allowed?(current_user, :"read_#{klass.to_ability_name}", project)

        project
      end
    end

    def project_id
      params[:project_id]
    end
  end
end
